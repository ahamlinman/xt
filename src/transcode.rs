//! Functions and types to support translation between Serde data formats.
//!
//! # Differences from `serde_transcode`
//!
//! [`serde_transcode`]: https://github.com/sfackler/serde-transcode
//!
//! jyt's transcoder is heavily based on the [`serde_transcode`] crate
//! advertised in the Serde documentation, with a few notable differences. **You
//! should prefer `serde_transcode` over `jyt::transcode`** if possible, as the
//! former is more widely used and its implementation is much simpler.
//!
//! `serde_transcode` stringifies serializer and deserializer errors as
//! necessary throughout the transcoding call stack to meet Serde API
//! requirements. At the cost of greater implementation complexity, jyt's
//! transcoder implements out of band mechanisms to preserve the original value
//! of the error that caused a transcode to fail, enabling more robust
//! inspection of error causes. The "Implementation Notes" section describes the
//! details of the error handling mechanism.
//!
//! `serde_transcode` directly exposes a `Transcoder` type that implements
//! `Serialize` for a `Deserializer`. jyt keeps this type private to avoid
//! creating a contract around the significantly more complex API associated
//! with the unique error handling implementation.
//!
//! jyt does not support transcoding `Option<T>` and newtype struct values, as
//! no jyt input format is expected to produce such values on its own. The
//! implementation could be extended to support this.
//!
//! # Implementation Notes
//!
//! These details are not important for usage, but are useful to understand the
//! concept of direct transcoding in Serde.
//!
//! The general premise of transcoding is that wherever a typical Serde client
//! would deserialize a value into a data structure, a transcoder "deserializes"
//! a value into the result (including side effects) produced by serializing
//! that value. Inversely, wherever a typical Serde client would serialize an
//! existing data structure into a value, a transcoder serializes whatever value
//! the deserializer sees in its input.
//!
//! The transcoding process relies on three special implementations of key Serde
//! traits:
//!
//! - A [`Visitor`](serde::de::Visitor) receives a single value from a
//! `Deserializer`, and either forwards a scalar value directly to the
//! serializer or recursively transcodes the elements of a sequence or map.
//!
//! - Various [`DeserializeSeed`] implementations receive `Deserializer`s for
//! each element of a sequence or map, and forward each value to the appropriate
//! method of a sequence or map serializer. For example, a `KeySeed` forwards
//! deserialized map keys to the `serialize_key` method of a [`SerializeMap`]
//! implementation.
//!
//! - A `Transcoder` implements [`Serialize`] for the [`Deserializer`] provided
//! to each seed, to meet the requirements of Serde's sequence and map
//! serializer APIs. It recursively constructs a `Visitor` to transcode the
//! collection element produced by the `Deserializer`.
//!
//! Each implementation is a single use state machine that owns some serializer
//! or deserializer instance in its "new" state, and that owns any error value
//! produced by that instance in its "used" state. For example, the new state of
//! a `Visitor` instance owns the `Serializer` to which the visited value will
//! be forwarded. When the deserializer invokes a `Visitor` method, it moves the
//! serializer out of the `Visitor` instance, consumes it in order to serialize
//! the visited value, and stores any error produced by the serializer into the
//! used state of the `Visitor` instance.
//!
//! The used states of each instance capture error values in order to preserve
//! them across Serde API boundaries. When a `Visitor` or `DeserializeSeed`
//! method encounters a _serializer_ error, Serde requires that it return an
//! error of a type defined by the _deserializer_ to safely halt transcoding.
//! Inversely, when the `serialize` method of a `Transcoder` encounters a
//! _deserializer_ error, it must return an error of a type defined by the
//! _serializer_. Within our method implementations, the `custom` functions that
//! construct these errors can only accept stringified values, and thus cannot
//! carry the full context of the original error. So, after capturing the
//! original error into its used state, each transcoder instance will simply
//! return a generic "translation failed" error of the required return type, and
//! expect the caller to take ownership of the original error by consuming the
//! instance.
//!
//! Unlike many Serde implementations, each instance of these implementations
//! **must only be used once over its lifetime**, and will panic if used more
//! than once. The transcoder carefully handles these internal types to uphold
//! the necessary expectations for usage and error handling.

use std::borrow::Cow;
use std::cell::Cell;
use std::error::Error;
use std::fmt;
use std::mem;

use serde::{
  de::{self, Deserialize, DeserializeSeed, Deserializer},
  ser::{self, Serialize, SerializeMap, SerializeSeq, Serializer},
};

/// The message used to generate generic serializer and deserializer errors.
const TRANSLATION_FAILED: &'static str = "translation failed";

/// Transcodes from a Serde `Deserializer` to a Serde `Serializer`.
///
/// The transcoder forwards the output produced by the deserializer directly to
/// the serializer without collecting it into an intermediate data structure. An
/// error on either side will halt further transcoding.
pub(crate) fn transcode<'de, D, S>(s: S, d: D) -> Result<S::Ok, TranscodeError<S::Error, D::Error>>
where
  S: Serializer,
  D: Deserializer<'de>,
{
  let mut visitor = Visitor::new(s);
  match d.deserialize_any(&mut visitor) {
    Ok(value) => Ok(value),
    Err(derr) => match visitor.into_serializer_error() {
      Some(serr) => Err(TranscodeError::Ser(serr, derr)),
      None => Err(TranscodeError::De(derr)),
    },
  }
}

/// Holds an error produced during transcoding.
#[derive(Debug)]
pub(crate) enum TranscodeError<S, D> {
  /// The serializer triggered the transcode failure, for example due to an
  /// input value it could not handle. The included deserializer error may
  /// provide useful context, such as the location of the value that the
  /// serializer could not handle.
  Ser(S, D),
  /// The deserializer triggered the transcode failure, for example due to
  /// invalid input.
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
      Ser(serr, derr) => write!(f, "{}: {}", derr, serr),
      De(derr) => fmt::Display::fmt(derr, f),
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
      Ser(serr, _) => Some(serr),
      De(derr) => Some(derr),
    }
  }
}

/// Enables a Serde `Deserializer` to drive the contained Serde `Serializer`.
///
/// See the module level documentation for special considerations associated
/// with usage of this type.
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
  fn new(s: S) -> Visitor<S> {
    Visitor::New(s)
  }

  fn take_serializer(&mut self) -> S {
    use Visitor::*;
    match mem::replace(self, Used(None)) {
      New(s) => s,
      Used(_) => panic!("visitor may only be used once"),
    }
  }

  fn capture(&mut self, serr: Option<S::Error>) {
    use Visitor::*;
    let old = mem::replace(self, Used(serr));
    if let Used(Some(_)) = old {
      panic!("visitor overwrote previous captured error");
    }
  }

  fn into_serializer_error(self) -> Option<S::Error> {
    use Visitor::*;
    match self {
      New(_) => None,
      Used(serr) => serr,
    }
  }
}

/// Implements the methods of a [`serde::de::Visitor`] that receive scalar
/// values from a deserializer and forward them directly to a serializer.
///
/// This macro is non-hygienic, and not intended for use outside of this module.
macro_rules! local_impl_transcode_visitor_methods {
  ($($visit:ident($($arg:ident: $ty:ty)?) => $serialize:ident;)*) => {
    $(
      fn $visit<E>(self, $($arg: $ty)?) -> Result<Self::Value, E>
      where
        E: de::Error,
      {
        match self.take_serializer().$serialize($($arg)?) {
          Ok(value) => Ok(value),
          Err(serr) => {
            self.capture(Some(serr));
            Err(E::custom(TRANSLATION_FAILED))
          }
        }
      }
    )*
  };
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
    use de::Error;

    let mut s = match self.take_serializer().serialize_seq(seq.size_hint()) {
      Ok(s) => s,
      Err(serr) => {
        self.capture(Some(serr));
        return Err(A::Error::custom(TRANSLATION_FAILED));
      }
    };

    loop {
      let mut seq_seed = SeqSeed::new(&mut s);
      match seq.next_element_seed(&mut seq_seed) {
        Ok(None) => break,
        Ok(Some(())) => {}
        Err(derr) => {
          self.capture(seq_seed.into_serializer_error());
          return Err(derr);
        }
      }
    }

    match s.end() {
      Ok(value) => Ok(value),
      Err(serr) => {
        self.capture(Some(serr));
        Err(A::Error::custom(TRANSLATION_FAILED))
      }
    }
  }

  fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
  where
    A: de::MapAccess<'de>,
  {
    use de::Error;

    let mut s = match self.take_serializer().serialize_map(map.size_hint()) {
      Ok(s) => s,
      Err(serr) => {
        self.capture(Some(serr));
        return Err(A::Error::custom(TRANSLATION_FAILED));
      }
    };

    loop {
      let mut key_seed = KeySeed::new(&mut s);
      match map.next_key_seed(&mut key_seed) {
        Ok(None) => break,
        Ok(Some(())) => {}
        Err(derr) => {
          self.capture(key_seed.into_serializer_error());
          return Err(derr);
        }
      }

      let mut value_seed = ValueSeed::new(&mut s);
      match map.next_value_seed(&mut value_seed) {
        Ok(()) => {}
        Err(derr) => {
          self.capture(value_seed.into_serializer_error());
          return Err(derr);
        }
      }
    }

    match s.end() {
      Ok(value) => Ok(value),
      Err(serr) => {
        self.capture(Some(serr));
        Err(A::Error::custom(TRANSLATION_FAILED))
      }
    }
  }
}

/// Implements `DeserializeSeed` types that participate in the transcoding of
/// sequence and map values.
///
/// This macro is non-hygienic, and not intended for use outside of this module.
macro_rules! local_impl_transcode_seed_types {
  ($($seed_name:ident => $ser_trait:ident :: $ser_method:ident;)*) => {
    $(
      enum $seed_name<'a, S>
      where
        S: $ser_trait,
      {
        New(&'a mut S),
        Used(Option<S::Error>),
      }

      impl<'a, S> $seed_name<'a, S>
      where
        S: $ser_trait,
      {
        fn new(s: &'a mut S) -> $seed_name<'a, S> {
          $seed_name::New(s)
        }

        fn take_serializer(&mut self) -> &'a mut S {
          use $seed_name::*;
          match mem::replace(self, Used(None)) {
            New(s) => s,
            Used(_) => panic!("seed may only be used once"),
          }
        }

        fn capture(&mut self, serr: Option<S::Error>) {
          use $seed_name::*;
          let old = mem::replace(self, Used(serr));
          if let Used(Some(_)) = old {
            panic!("seed overwrote previous captured error");
          }
        }

        fn into_serializer_error(self) -> Option<S::Error> {
          use $seed_name::*;
          match self {
            New(_) => None,
            Used(serr) => serr,
          }
        }
      }

      impl<'a, 'de, S> DeserializeSeed<'de> for &mut $seed_name<'a, S>
      where
        S: $ser_trait,
      {
        type Value = ();

        fn deserialize<D>(self, d: D) -> Result<Self::Value, D::Error>
        where
          D: Deserializer<'de>,
        {
          use de::Error;

          let transcoder = Transcoder::new(d);
          match self.take_serializer().$ser_method(&transcoder) {
            Ok(()) => Ok(()),
            Err(serr) => {
              self.capture(Some(serr));
              match transcoder.into_deserializer_error() {
                Some(derr) => Err(derr),
                None => Err(D::Error::custom(TRANSLATION_FAILED)),
              }
            }
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
/// See the module level documentation for special considerations associated
/// with usage of this type.
///
/// # Panics
///
/// Panics if `serialize` is invoked more than once.
struct Transcoder<'de, D>(Cell<TranscoderState<'de, D>>)
where
  D: Deserializer<'de>;

enum TranscoderState<'de, D>
where
  D: Deserializer<'de>,
{
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

  fn take_deserializer(&self) -> D {
    use TranscoderState::*;
    match self.0.replace(Used(None)) {
      New(d) => d,
      Used(_) => panic!("transcoder may only be used once"),
    }
  }

  fn capture(&self, derr: Option<D::Error>) {
    use TranscoderState::*;
    let old = self.0.replace(Used(derr));
    if let Used(Some(_)) = old {
      panic!("transcoder overwrote previous captured error");
    }
  }

  fn into_deserializer_error(self) -> Option<D::Error> {
    use TranscoderState::*;
    match self.0.into_inner() {
      New(_) => None,
      Used(derr) => derr,
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

    let mut visitor = Visitor::new(s);
    match self.take_deserializer().deserialize_any(&mut visitor) {
      Ok(value) => Ok(value),
      Err(derr) => {
        self.capture(Some(derr));
        match visitor.into_serializer_error() {
          Some(serr) => Err(serr),
          None => Err(S::Error::custom(TRANSLATION_FAILED)),
        }
      }
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
