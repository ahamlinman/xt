use std::borrow::Cow;
use std::cell::RefCell;
use std::error::Error;
use std::fmt;

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
pub fn transcode<'de, D, S>(d: D, s: S) -> Result<S::Ok, TranscodeError<S::Error, D::Error>>
where
  D: Deserializer<'de>,
  S: Serializer,
{
  use TranscodeError::*;
  match d.deserialize_any(Visitor(s)) {
    Ok(ser_result) => match ser_result {
      Ok(ser_value) => Ok(ser_value),
      Err(ser_err) => Err(Ser(ser_err)),
    },
    Err(de_err) => Err(De(de_err)),
  }
}

/// Holds an error produced during transcoding.
#[derive(Debug)]
pub enum TranscodeError<S, D> {
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

/// Implements methods of a [`serde::de::Visitor`] that do nothing more than
/// shove the result of some expression into an `Ok` variant, using terms of the
/// following form:
///
/// ```
/// visit_<type>(<args>) => <return value>;
/// ```
///
/// This macro is non-hygienic, and not intended for use outside of this module.
macro_rules! local_impl_infallible_visitor_methods {
  ($($name:ident($($args:tt)*) => $result:expr;)*) => {
    $(
      fn $name<E: de::Error>($($args)*) -> Result<Self::Value, E> {
        Ok($result)
      }
    )*
  };
}

/// Enables a Serde `Deserializer` to drive the contained Serde `Serializer`.
///
/// Where a normal visitor would deserialize valid input to some in-memory
/// representation, this visitor effectively "deserializes" valid input to the
/// `Result` produced by an attempt to serialize it.
///
/// When the serializer produces an error, we stop taking input from the
/// deserializer and pass the error up the call stack, where it becomes the
/// final value. Note that the Serde documentation does not indicate any
/// requirement for a visitor to access every element of a sequence or map
/// before returning a value.
struct Visitor<S>(S);

impl<'de, S> de::Visitor<'de> for Visitor<S>
where
  S: Serializer,
{
  type Value = Result<S::Ok, S::Error>;

  fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    write!(fmt, "any supported value")
  }

  local_impl_infallible_visitor_methods! {
    visit_unit(self) => self.0.serialize_unit();

    visit_bool(self, v: bool) => self.0.serialize_bool(v);

    visit_i8(self, v: i8) => self.0.serialize_i8(v);
    visit_i16(self, v: i16) => self.0.serialize_i16(v);
    visit_i32(self, v: i32) => self.0.serialize_i32(v);
    visit_i64(self, v: i64) => self.0.serialize_i64(v);
    visit_i128(self, v: i128) => self.0.serialize_i128(v);

    visit_u8(self, v: u8) => self.0.serialize_u8(v);
    visit_u16(self, v: u16) => self.0.serialize_u16(v);
    visit_u32(self, v: u32) => self.0.serialize_u32(v);
    visit_u64(self, v: u64) => self.0.serialize_u64(v);
    visit_u128(self, v: u128) => self.0.serialize_u128(v);

    visit_f32(self, v: f32) => self.0.serialize_f32(v);
    visit_f64(self, v: f64) => self.0.serialize_f64(v);

    visit_char(self, v: char) => self.0.serialize_char(v);
    visit_str(self, v: &str) => self.0.serialize_str(v);
    visit_bytes(self, v: &[u8]) => self.0.serialize_bytes(v);
  }

  fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
  where
    A: de::SeqAccess<'de>,
  {
    let mut ser = match self.0.serialize_seq(seq.size_hint()) {
      Ok(ser) => ser,
      Err(err) => return Ok(Err(err)),
    };

    while let Some(result) = seq.next_element_seed(SeqSeed(&mut ser))? {
      if let Err(err) = result {
        return Ok(Err(err));
      }
    }

    Ok(ser.end())
  }

  fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
  where
    A: de::MapAccess<'de>,
  {
    let mut ser = match self.0.serialize_map(map.size_hint()) {
      Ok(ser) => ser,
      Err(err) => return Ok(Err(err)),
    };

    while let Some(result) = map.next_key_seed(KeySeed(&mut ser))? {
      if let Err(err) = result {
        return Ok(Err(err));
      }
      if let Err(err) = map.next_value_seed(ValueSeed(&mut ser))? {
        return Ok(Err(err));
      }
    }

    Ok(ser.end())
  }
}

/// Implements deserializer seed types that use a `Transcoder` to handle
/// sequence and map elements.
///
/// This macro is non-hygienic, and not intended for use outside of this module.
macro_rules! local_impl_transcode_seed_type {
  ($name:ident, $serializer_trait:ident, $serializer_method:ident) => {
    struct $name<'a, S: 'a>(&'a mut S);

    impl<'a, 'de, S> de::DeserializeSeed<'de> for $name<'a, S>
    where
      S: $serializer_trait,
    {
      type Value = Result<(), S::Error>;

      fn deserialize<D>(self, d: D) -> Result<Self::Value, D::Error>
      where
        D: Deserializer<'de>,
      {
        let t = Transcoder::new(d);
        match self.0.$serializer_method(&t) {
          Ok(_) => Ok(Ok(())),
          // See Transcoder documentation for an explanation of this part.
          Err(serr) => match t.into_error() {
            Some(derr) => Err(derr),
            None => Ok(Err(serr)),
          },
        }
      }
    }
  };
}

local_impl_transcode_seed_type!(SeqSeed, SerializeSeq, serialize_element);
local_impl_transcode_seed_type!(KeySeed, SerializeMap, serialize_key);
local_impl_transcode_seed_type!(ValueSeed, SerializeMap, serialize_value);

/// Implements `Serialize` for a `Deserializer`, allowing our transcoding
/// visitor to forward complex values like sequences and maps.
///
/// **This is not a normal `Serialize` implementation!** Unlike most data types,
/// serialization of a transcoder is not idempotent, as it advances the state of
/// the contained deserializer. Serializing a transcoder more than once will
/// panic.
///
/// On deserializer errors, a transcoder will store the original error
/// internally, and return a fairly useless and generic serialization error.
/// When `serialize` returns an error, typical usage is to first check for and
/// return any underlying deserializer error before propagating the original
/// serialization error. If there is no captured deserializer error, then the
/// error was produced by the serializer itself.
struct Transcoder<'de, D: Deserializer<'de>>(RefCell<TranscoderState<'de, D>>);

enum TranscoderState<'de, D: Deserializer<'de>> {
  New(Option<D>),
  Used(Option<D::Error>),
}

impl<'de, D> Transcoder<'de, D>
where
  D: Deserializer<'de>,
{
  /// Constructs a new `Transcoder`.
  fn new(d: D) -> Transcoder<'de, D> {
    Transcoder(RefCell::new(TranscoderState::New(Some(d))))
  }

  /// Consumes a `Transcoder` and produces any error generated by the
  /// deserializer. If this is `None`, then any error returned by `serialize`
  /// was produced by the serializer itself.
  fn into_error(self) -> Option<D::Error> {
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
  /// Transcodes the contained deserializer.
  ///
  /// **This is not a normal `Serialize` implementation!** See the struct level
  /// documentation for special usage considerations.
  fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    use ser::Error;
    use TranscoderState::*;

    let de_result = match *self.0.borrow_mut() {
      Used(_) => panic!("transcoder may only be serialized once"),
      New(ref mut d) => {
        let d = d.take().unwrap();
        d.deserialize_any(Visitor(s))
      }
    };

    match de_result {
      Ok(ser_result) => {
        *self.0.borrow_mut() = Used(None);
        ser_result
      }
      Err(de_err) => {
        *self.0.borrow_mut() = Used(Some(de_err));
        Err(S::Error::custom(
          "deserializer error, must unpack from transcoder",
        ))
      }
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
///   hash table.
pub enum Value<'a> {
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

      local_impl_infallible_visitor_methods! {
        visit_unit(self) => Value::None;

        visit_bool(self, v: bool) => Value::Bool(v);

        visit_i8(self, v: i8) => Value::I8(v);
        visit_i16(self, v: i16) => Value::I16(v);
        visit_i32(self, v: i32) => Value::I32(v);
        visit_i64(self, v: i64) => Value::I64(v);
        visit_i128(self, v: i128) => Value::I128(v);

        visit_u8(self, v: u8) => Value::U8(v);
        visit_u16(self, v: u16) => Value::U16(v);
        visit_u32(self, v: u32) => Value::U32(v);
        visit_u64(self, v: u64) => Value::U64(v);
        visit_u128(self, v: u128) => Value::U128(v);

        visit_f32(self, v: f32) => Value::F32(v);
        visit_f64(self, v: f64) => Value::F64(v);

        visit_char(self, v: char) => Value::Char(v);

        visit_str(self, v: &str) => Value::String(Cow::Owned(v.to_owned()));
        visit_borrowed_str(self, v: &'a str) => Value::String(Cow::Borrowed(v));
        visit_string(self, v: String) => Value::String(Cow::Owned(v));

        visit_bytes(self, v: &[u8]) => Value::Bytes(Cow::Owned(v.to_owned()));
        visit_borrowed_bytes(self, v: &'a [u8]) => Value::Bytes(Cow::Borrowed(v));
        visit_byte_buf(self, v: Vec<u8>) => Value::Bytes(Cow::Owned(v));
      }

      fn visit_seq<V: de::SeqAccess<'a>>(self, mut v: V) -> Result<Self::Value, V::Error> {
        let mut vec = match v.size_hint() {
          None => Vec::new(),
          Some(s) => Vec::with_capacity(s),
        };
        while let Some(e) = v.next_element()? {
          vec.push(e)
        }
        Ok(Value::Seq(vec))
      }

      fn visit_map<V: de::MapAccess<'a>>(self, mut v: V) -> Result<Self::Value, V::Error> {
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
