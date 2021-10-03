use std::borrow::Cow;
use std::cell::Cell;
use std::error::Error;
use std::fmt;
use std::mem;

use serde::{
  de::{self, Deserialize, Deserializer},
  ser::{self, Serialize, SerializeMap, SerializeSeq, Serializer},
};

/// Transcodes from a Serde `Deserializer` to a Serde `Serializer`.
///
/// Values produced by the `Deserializer` will be forwarded directly to the
/// `Serializer` without collecting the input into an intermediate form in
/// memory. An error on either side will halt further transcoding.
///
/// The implementation is heavily based on a general strategy pioneered by the
/// `serde_transcode` crate, with modifications to preserve `Serializer` and
/// `Deserializer` error types rather than stringifying them. It only implements
/// a subset of the Serde types that `serde_transcode` does, focusing solely on
/// the types that one of jyt's input formats could reasonably produce (e.g. no
/// options or newtype structs), however the general implementation pattern
/// could be extended to support such types.
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
/// Where a normal visitor implementation would deserialize to some in-memory
/// value, this implementation deserializes to the `Ok` type of the contained
/// serializer. Unlike some visitors, it can only be used once, as it advances
/// the state of the contained serializer.
///
/// # Panics
///
/// Panics if used more than once. Any call to an implemented visitor method
/// counts as a single use.
///
/// # Error Handling
///
/// Serde requires that visitor methods return errors of some type provided by
/// the deserializer, for which we only have a `custom` constructor that takes
/// `Display` values. When a visitor fails due to an error in the serializer, we
/// cannot propagate the error normally without losing information.
///
/// When a serializer generates an error, a visitor stores that original error
/// and returns a deserializer error with a generic message. To handle a
/// deserializer error, first handle any original error in the `Some` variant of
/// `into_serializer_error`. If this is a `None` variant, then the error was
/// produced by the deserializer and should be handled as is.
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
            Err(E::custom("serializer error, must unpack from visitor"))
          }
        }
      }
    )*
  }
}

/// Stores a serializer error into a visitor, then returns a generic
/// deserializer error.
///
/// This macro is non-hygienic, and not intended for use outside of this module.
macro_rules! local_visitor_ser_error {
  ($self:ident, $err:expr) => {{
    *$self = Visitor::Used(Some($err));
    use serde::de::Error;
    Err(A::Error::custom(
      "serializer error, must unpack from visitor",
    ))
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
/// Serde deserializers drive `DeserializeSeed` implementations to decode each
/// element of a sequence or map as a deserializable value. Where a normal
/// `DeserializeSeed` implementation would deserialize a valid element to some
/// in-memory representation, this deserializes each element to any error
/// produced by attempting to serialize it with the corresponding method of a
/// `SerializeSeq` or `SerializeMap` implementation.
///
/// This macro is non-hygienic, and not intended for use outside of this module.
macro_rules! local_impl_transcode_seed_types {
  ($($seed_name:ident => $ser_trait:ident :: $ser_method:ident;)*) => {
    $(
      struct $seed_name<'a, S: 'a>(&'a mut S);

      impl<'a, 'de, S> de::DeserializeSeed<'de> for $seed_name<'a, S>
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
/// Where a normal `Serialize` implementation would serialize some in-memory
/// value, this implementation serializes whatever the contained deserializer
/// generates. Unlike most data types, a transcoder can only be serialized once,
/// as it advances the state of the contained deserializer.
///
/// # Panics
///
/// Panics if `serialize` is called more than once.
///
/// # Error Handling
///
/// Serde requires that `serialize` return errors of the serializer's associated
/// error type, for which we only have a `custom` constructor that takes
/// `Display` values. When `serialize` fails due to an error in the
/// deserializer, we cannot propagate the error normally without losing
/// information.
///
/// When a deserializer generates an error, a transcoder stores that original
/// error and returns a serializer error with a generic message. To handle a
/// `serialize` error, first handle any original error in the `Some` variant of
/// `into_deserializer_error`. If this is a `None` variant, then the error was
/// produced by the serializer and should be handled as is.
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
          Err(S::Error::custom(
            "deserializer error, must unpack from transcoder",
          ))
        }
      },
    }
  }
}

/// Temporarily represents a deserialized value in memory.
///
/// On occasion, a Serde data format will not allow direct access to a usable
/// `Serializer` or `Deserializer` for use with the [`transcode`] function, but
/// will instead require the use of an intermediate (de)serializable value.
/// `Value` is optimized to support this use case within jyt, distinguishing
/// itself from alternative implementations by:
///
/// - Faithfully representing all Serde types that a jyt input format could
///   produce (all integer sizes, raw bytes), but no more (options, etc.).
/// - Borrowing from the deserializer whenever possible, avoiding new
///   allocations but limiting the lifetime of the value.
/// - Representing maps as a vector of 2-tuples, preserving the original order
///   of values but avoiding the features (and overhead) of an order-preserving
///   hash map.
pub(crate) enum Value<'a> {
  None,
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
      None => s.serialize_unit(),
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
        visit_unit() => Value::None;

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
