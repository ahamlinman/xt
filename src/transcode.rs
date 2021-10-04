//! Functions and types to support translation between Serde data formats.
//!
//! # Implementation Notes
//!
//! These details are not important for usage, but are useful to understand the
//! implementation of direct transcoding. jyt's transcoder is heavily based on
//! the [`serde_transcode`] crate advertised in the Serde documentation, with
//! some notable differences discussed later in this section.
//!
//! [`serde_transcode`]: https://github.com/sfackler/serde-transcode
//!
//! ## Principles of Transcoding
//!
//! A Serde [`Deserializer`] drives the transcoding process as it parses some
//! input and discovers what it contains. The [`Visitor`](serde::de::Visitor)
//! trait defines the interface that a deserializer invokes to provide a single
//! "next" input value to its client. Where a more typical `Visitor` would build
//! some data structure from the value it receives, we instead implement a
//! `Visitor` that forwards that value to a [`Serializer`], yielding a value of
//! the serializer's `Ok` type on success. For scalar values like integers and
//! strings, this is the only type involved in the transcoding process.
//!
//! Serde's API and data model treats more complex structures, like sequences
//! and maps, as collections of deserializable values. To access each element of
//! a collection, we provide an implementation of the [`DeserializeSeed`] trait
//! to the deserializer. Unlike the standard [`Deserialize`] trait, a seed
//! accepts `self` in its `deserialize` method and can produce a value of a type
//! other than `Self`. Our seeds will forward the deserialized values of a
//! collection to some collection-specific serializer type, such as a
//! [`SerializeSeq`], and yield any error that the serializer produces.
//!
//! Because the sequence and map serializer APIs accept values implementing the
//! `Serialize` trait, the transcoding puzzle is completed by implementing
//! `Serialize` for the `Deserializer` provided to each seed's `deserialize`
//! method. Where a typical serializable value would drive a serializer based on
//! a data structure, this `Transcoder` type drives it from the values produced
//! by the deserializer, using a new instance of our `Visitor` type that will
//! recursively continue the transcoding process.
//!
//! Unlike with many Serde-compatible data structures, **a single `Visitor` or
//! `Transcoder` must only be used once over its lifetime**, as the transcoding
//! process consumes from the deserializer without persisting any values it
//! yields. The transcoding process carefully handles these internal types to
//! uphold this requirement.
//!
//! ## Error Handling
//!
//! When a `Visitor` method encounters a serializer error, Serde requires that
//! it return an error of a type defined by the _deserializer_ to safely halt
//! transcoding. Inversely, when the `serialize` method of a `Transcoder`
//! encounters a deserializer error, it must return an error of a type defined
//! by the _serializer_. From within the visitor and transcoder implementations,
//! the methods that construct these error values are incapable of passing on
//! the original error of the inverse type, which may contain context that we
//! wish to preserve for the caller.
//!
//! Visitor and transcoder methods that cannot return a given error directly
//! will capture it internally and return a generic error of the appropriate
//! type in its place. Callers that handle visitor or transcoder errors can take
//! ownership of any captured error by consuming the used visitor or transcoder,
//! and are expected to handle such captured errors in place of the original.
//!
//! ## Differences from `serde_transcode`
//!
//! The above error handling mechanism is unique to jyt. `serde_transcode`
//! stringifies errors as necessary throughout the call stack of the transcode
//! operation, eventually yielding an error of the serializer's type. While this
//! may erase useful context from the original error, such as the details of a
//! failed I/O operation in the serializer, it can provide more useful output
//! for end users. For example, the deserializer's errors may indicate the line
//! and column of a value in the source file that the serializer was unable to
//! handle. jyt would need an additional out of band mechanism to capture this
//! detail at the right point in the call stack.
//!
//! `serde_transcode` directly exposes the `Transcoder` type that implements
//! `Serialize` for a `Deserializer`. jyt keeps this type private to avoid
//! creating an API contract around the complex error handling semantics.
//!
//! jyt does not support transcoding `Option<T>` and newtype struct values, as
//! no jyt input format is expected to produce such values on its own. The
//! implementation could be extended to support this.

use std::borrow::Cow;
use std::cell::Cell;
use std::error::Error;
use std::fmt;
use std::mem;

use serde::{
  de::{self, Deserialize, DeserializeSeed, Deserializer},
  ser::{self, Serialize, SerializeMap, SerializeSeq, Serializer},
};

/// Transcodes from a Serde `Deserializer` to a Serde `Serializer`.
///
/// Values produced by the `Deserializer` will be forwarded directly to the
/// `Serializer` without collecting the input into an intermediate form in
/// memory. An error on either side will halt further transcoding.
pub(crate) fn transcode<'de, D, S>(
  ser: S,
  de: D,
) -> Result<S::Ok, TranscodeError<S::Error, D::Error>>
where
  S: Serializer,
  D: Deserializer<'de>,
{
  use TranscodeError::*;
  let mut visitor = Visitor::new(ser);
  match de.deserialize_any(&mut visitor) {
    Ok(value) => Ok(value),
    Err(derr) => match visitor.into_serializer_error() {
      Some(serr) => Err(Ser(serr)),
      None => Err(De(derr)),
    },
  }
}

/// Holds an error produced during transcoding.
#[derive(Debug)]
pub(crate) enum TranscodeError<S, D> {
  Ser(S),
  De(D),
}

impl<S, D> fmt::Display for TranscodeError<S, D>
where
  S: ser::Error,
  D: de::Error,
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    use TranscodeError::*;
    match self {
      Ser(err) => fmt::Display::fmt(err, f),
      De(err) => fmt::Display::fmt(err, f),
    }
  }
}

impl<S, D> Error for TranscodeError<S, D>
where
  S: ser::Error + 'static,
  D: de::Error + 'static,
{
  fn source(&self) -> Option<&(dyn Error + 'static)> {
    use TranscodeError::*;
    match self {
      Ser(err) => Some(err),
      De(err) => Some(err),
    }
  }
}

/// Enables a Serde `Deserializer` to drive the contained Serde `Serializer`.
///
/// See the module level documentation for details of this type and its expected
/// usage.
///
/// # Panics
///
/// Panics if used more than once. Any call to an implemented visitor method
/// counts as a single use.
enum Visitor<S>
where
  S: Serializer,
{
  New(S),
  Used(Option<S::Error>),
}

impl<S> Visitor<S>
where
  S: Serializer,
{
  fn new(ser: S) -> Visitor<S> {
    Visitor::New(ser)
  }

  fn take_serializer(&mut self) -> S {
    use Visitor::*;
    match mem::replace(self, Used(None)) {
      New(ser) => ser,
      Used(_) => panic!("visitor may only be used once"),
    }
  }

  fn into_serializer_error(self) -> Option<S::Error> {
    use Visitor::*;
    match self {
      New(_) => None,
      Used(err) => err,
    }
  }
}

/// Implements methods of a [`serde::de::Visitor`] for our transcoding visitor.
///
/// This macro is non-hygienic, and not intended for use outside of this module.
macro_rules! local_impl_transcode_visitor_methods {
  ($($visit:ident($($arg:ident: $ty:ty)?) => $serialize:ident;)*) => {
    $(
      fn $visit<E: de::Error>(self, $($arg: $ty)?) -> Result<Self::Value, E> {
        match self.take_serializer().$serialize($($arg)?) {
          Ok(value) => Ok(value),
          Err(err) => {
            *self = Visitor::Used(Some(err));
            Err(E::custom("translation failed"))
          }
        }
      }
    )*
  }
}

/// Stores a serializer error generated while visiting a sequence or map into a
/// visitor, then returns a generic deserializer error.
///
/// This macro is non-hygienic, and not intended for use outside of this module.
macro_rules! local_visitor_ser_error {
  ($self:ident, $err:expr) => {{
    *$self = Visitor::Used(Some($err));
    use serde::de::Error;
    Err(A::Error::custom("translation failed"))
  }};
}

impl<'de, S> de::Visitor<'de> for &mut Visitor<S>
where
  S: Serializer,
{
  type Value = S::Ok;

  fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    write!(fmt, "any supported value")
  }

  local_impl_transcode_visitor_methods! {
    visit_unit() => serialize_unit;
    visit_bool(v: bool) => serialize_bool;

    visit_i8(v: i8) => serialize_i8;
    visit_i16(v: i16) => serialize_i16;
    visit_i32(v: i32) => serialize_i32;
    visit_i64(v: i64) => serialize_i64;
    visit_i128(v: i128) => serialize_i128;

    visit_u8(v: u8) => serialize_u8;
    visit_u16(v: u16) => serialize_u16;
    visit_u32(v: u32) => serialize_u32;
    visit_u64(v: u64) => serialize_u64;
    visit_u128(v: u128) => serialize_u128;

    visit_f32(v: f32) => serialize_f32;
    visit_f64(v: f64) => serialize_f64;

    visit_char(v: char) => serialize_char;
    visit_str(v: &str) => serialize_str;
    visit_bytes(v: &[u8]) => serialize_bytes;
  }

  fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
  where
    A: de::SeqAccess<'de>,
  {
    let mut ser = match self.take_serializer().serialize_seq(seq.size_hint()) {
      Ok(ser) => ser,
      Err(err) => return local_visitor_ser_error!(self, err),
    };

    while let Some(result) = seq.next_element_seed(SeqSeed(&mut ser))? {
      if let Some(err) = result {
        return local_visitor_ser_error!(self, err);
      }
    }

    match ser.end() {
      Ok(v) => Ok(v),
      Err(err) => local_visitor_ser_error!(self, err),
    }
  }

  fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
  where
    A: de::MapAccess<'de>,
  {
    let mut ser = match self.take_serializer().serialize_map(map.size_hint()) {
      Ok(ser) => ser,
      Err(err) => return local_visitor_ser_error!(self, err),
    };

    while let Some(result) = map.next_key_seed(KeySeed(&mut ser))? {
      if let Some(err) = result {
        return local_visitor_ser_error!(self, err);
      }
      if let Some(err) = map.next_value_seed(ValueSeed(&mut ser))? {
        return local_visitor_ser_error!(self, err);
      }
    }

    match ser.end() {
      Ok(v) => Ok(v),
      Err(err) => local_visitor_ser_error!(self, err),
    }
  }
}

/// Implements `DeserializeSeed` types for methods of sequence and map
/// serializers.
///
/// See the module level documentation for details of how these types are used.
///
/// This macro is non-hygienic, and not intended for use outside of this module.
macro_rules! local_impl_transcode_seed_types {
  ($($seed_name:ident => $ser_trait:ident :: $ser_method:ident;)*) => {
    $(
      struct $seed_name<'a, S: 'a>(&'a mut S);

      impl<'a, 'de, S> DeserializeSeed<'de> for $seed_name<'a, S>
      where
        S: $ser_trait,
      {
        type Value = Option<S::Error>;

        fn deserialize<D>(self, de: D) -> Result<Self::Value, D::Error>
        where
          D: Deserializer<'de>,
        {
          let transcoder = Transcoder::new(de);
          match self.0.$ser_method(&transcoder) {
            Ok(()) => Ok(None),
            Err(serr) => match transcoder.into_deserializer_error() {
              Some(derr) => Err(derr),
              None => Ok(Some(serr)),
            },
          }
        }
      }
    )*
  };
}

local_impl_transcode_seed_types! {
  SeqSeed => SerializeSeq::serialize_element;
  KeySeed => SerializeMap::serialize_key;
  ValueSeed => SerializeMap::serialize_value;
}

/// Implements `Serialize` for a `Deserializer`.
///
/// See the module level documentation for details of this type and its expected
/// usage.
///
/// # Panics
///
/// Panics if `serialize` is invoked more than once.
struct Transcoder<'de, D: Deserializer<'de>>(Cell<TranscoderState<'de, D>>);

enum TranscoderState<'de, D: Deserializer<'de>> {
  New(D),
  Used(Option<D::Error>),
}

impl<'de, D> Transcoder<'de, D>
where
  D: Deserializer<'de>,
{
  fn new(d: D) -> Transcoder<'de, D> {
    Transcoder(Cell::new(TranscoderState::New(d)))
  }

  fn into_deserializer_error(self) -> Option<D::Error> {
    use TranscoderState::*;
    match self.0.into_inner() {
      New(_) => None,
      Used(result) => result,
    }
  }
}

impl<'de, D> ser::Serialize for Transcoder<'de, D>
where
  D: Deserializer<'de>,
{
  fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    use ser::Error;
    use TranscoderState::*;

    let de = match self.0.replace(Used(None)) {
      New(de) => de,
      Used(_) => panic!("transcoder may only be serialized once"),
    };

    let mut visitor = Visitor::new(s);
    match de.deserialize_any(&mut visitor) {
      Ok(value) => Ok(value),
      Err(derr) => match visitor.into_serializer_error() {
        Some(serr) => Err(serr),
        None => {
          self.0.set(Used(Some(derr)));
          Err(S::Error::custom("translation failed"))
        }
      },
    }
  }
}

/// A deserialized value referencing borrowed data.
///
/// In some cases, a jyt input format may not be able to provide a
/// `Deserializer` for use with the [`transcode`] function, and must instead
/// deserialize to a data structure. `Value` uses simple data structures along
/// with Serde's famous zero copy deserialization to facilitate this use case
/// more efficiently than with typical generic value types, at the cost of the
/// greater flexibility such types often provide.
pub(crate) enum Value<'a> {
  Unit,
  Bool(bool),
  I8(i8),
  I16(i16),
  I32(i32),
  I64(i64),
  I128(i128),
  U8(u8),
  U16(u16),
  U32(u32),
  U64(u64),
  U128(u128),
  F32(f32),
  F64(f64),
  Char(char),
  String(Cow<'a, str>),
  Bytes(Cow<'a, [u8]>),
  Seq(Vec<Value<'a>>),
  Map(Vec<(Value<'a>, Value<'a>)>),
}

impl Serialize for Value<'_> {
  fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    use Value::*;
    match self {
      Unit => s.serialize_unit(),
      Bool(b) => s.serialize_bool(*b),
      I8(n) => s.serialize_i8(*n),
      I16(n) => s.serialize_i16(*n),
      I32(n) => s.serialize_i32(*n),
      I64(n) => s.serialize_i64(*n),
      I128(n) => s.serialize_i128(*n),
      U8(n) => s.serialize_u8(*n),
      U16(n) => s.serialize_u16(*n),
      U32(n) => s.serialize_u32(*n),
      U64(n) => s.serialize_u64(*n),
      U128(n) => s.serialize_u128(*n),
      F32(f) => s.serialize_f32(*f),
      F64(f) => s.serialize_f64(*f),
      Char(c) => s.serialize_char(*c),
      String(v) => v.serialize(s),
      Bytes(v) => v.serialize(s),
      Seq(v) => v.serialize(s),
      Map(m) => {
        let mut map = s.serialize_map(Some(m.len()))?;
        for (k, v) in m {
          map.serialize_entry(k, v)?;
        }
        map.end()
      }
    }
  }
}

/// Implements methods of a [`serde::de::Visitor`] that do nothing more than
/// construct a value and shove it into an `Ok` variant.
///
/// This macro is non-hygienic, and not intended for use outside of this module.
macro_rules! local_impl_value_visitor_methods {
  ($($name:ident($($arg:ident: $ty:ty)?) => $result:expr;)*) => {
    $(
      fn $name<E: de::Error>(self, $($arg: $ty)?) -> Result<Self::Value, E> {
        Ok($result)
      }
    )*
  };
}

impl<'de: 'a, 'a> Deserialize<'de> for Value<'a> {
  fn deserialize<D>(d: D) -> Result<Self, D::Error>
  where
    D: Deserializer<'de>,
  {
    struct Visitor;

    impl<'a> de::Visitor<'a> for Visitor {
      type Value = Value<'a>;

      fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "any supported value")
      }

      local_impl_value_visitor_methods! {
        visit_unit() => Value::Unit;

        visit_bool(v: bool) => Value::Bool(v);

        visit_i8(v: i8) => Value::I8(v);
        visit_i16(v: i16) => Value::I16(v);
        visit_i32(v: i32) => Value::I32(v);
        visit_i64(v: i64) => Value::I64(v);
        visit_i128(v: i128) => Value::I128(v);

        visit_u8(v: u8) => Value::U8(v);
        visit_u16(v: u16) => Value::U16(v);
        visit_u32(v: u32) => Value::U32(v);
        visit_u64(v: u64) => Value::U64(v);
        visit_u128(v: u128) => Value::U128(v);

        visit_f32(v: f32) => Value::F32(v);
        visit_f64(v: f64) => Value::F64(v);

        visit_char(v: char) => Value::Char(v);

        visit_str(v: &str) => Value::String(Cow::Owned(v.to_owned()));
        visit_borrowed_str(v: &'a str) => Value::String(Cow::Borrowed(v));
        visit_string(v: String) => Value::String(Cow::Owned(v));

        visit_bytes(v: &[u8]) => Value::Bytes(Cow::Owned(v.to_owned()));
        visit_borrowed_bytes(v: &'a [u8]) => Value::Bytes(Cow::Borrowed(v));
        visit_byte_buf(v: Vec<u8>) => Value::Bytes(Cow::Owned(v));
      }

      fn visit_seq<A: de::SeqAccess<'a>>(self, mut v: A) -> Result<Self::Value, A::Error> {
        let mut vec = match v.size_hint() {
          None => Vec::new(),
          Some(s) => Vec::with_capacity(s),
        };
        while let Some(e) = v.next_element()? {
          vec.push(e)
        }
        Ok(Value::Seq(vec))
      }

      fn visit_map<A: de::MapAccess<'a>>(self, mut v: A) -> Result<Self::Value, A::Error> {
        let mut vec = match v.size_hint() {
          None => Vec::new(),
          Some(s) => Vec::with_capacity(s),
        };
        while let Some(entry) = v.next_entry()? {
          vec.push(entry)
        }
        Ok(Value::Map(vec))
      }
    }

    d.deserialize_any(Visitor)
  }
}
