use std::borrow::Cow;
use std::cell::RefCell;
use std::error::Error;
use std::fmt;

use serde::{
  de::{self, Deserialize, Deserializer},
  ser::{self, Serialize, SerializeMap, SerializeSeq, Serializer},
};

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

macro_rules! __impl_visit_for_value {
  ($ty:ty, $visit:ident, $variant:expr) => {
    fn $visit<E: de::Error>(self, v: $ty) -> Result<Value<'a>, E> {
      Ok($variant(v))
    }
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

      fn visit_unit<E: de::Error>(self) -> Result<Value<'a>, E> {
        Ok(Value::None)
      }

      __impl_visit_for_value!(bool, visit_bool, Value::Bool);
      __impl_visit_for_value!(i8, visit_i8, Value::I8);
      __impl_visit_for_value!(i16, visit_i16, Value::I16);
      __impl_visit_for_value!(i32, visit_i32, Value::I32);
      __impl_visit_for_value!(i64, visit_i64, Value::I64);
      __impl_visit_for_value!(i128, visit_i128, Value::I128);
      __impl_visit_for_value!(u8, visit_u8, Value::U8);
      __impl_visit_for_value!(u16, visit_u16, Value::U16);
      __impl_visit_for_value!(u32, visit_u32, Value::U32);
      __impl_visit_for_value!(u64, visit_u64, Value::U64);
      __impl_visit_for_value!(u128, visit_u128, Value::U128);
      __impl_visit_for_value!(f32, visit_f32, Value::F32);
      __impl_visit_for_value!(f64, visit_f64, Value::F64);
      __impl_visit_for_value!(char, visit_char, Value::Char);

      fn visit_str<E: de::Error>(self, v: &str) -> Result<Value<'a>, E> {
        Ok(Value::String(Cow::Owned(v.to_owned())))
      }

      fn visit_borrowed_str<E: de::Error>(self, v: &'a str) -> Result<Value<'a>, E> {
        Ok(Value::String(Cow::Borrowed(v)))
      }

      fn visit_string<E: de::Error>(self, v: String) -> Result<Value<'a>, E> {
        Ok(Value::String(Cow::Owned(v)))
      }

      fn visit_bytes<E: de::Error>(self, v: &[u8]) -> Result<Value<'a>, E> {
        Ok(Value::Bytes(Cow::Owned(v.to_owned())))
      }

      fn visit_borrowed_bytes<E: de::Error>(self, v: &'a [u8]) -> Result<Value<'a>, E> {
        Ok(Value::Bytes(Cow::Borrowed(v)))
      }

      fn visit_byte_buf<E: de::Error>(self, v: Vec<u8>) -> Result<Value<'a>, E> {
        Ok(Value::Bytes(Cow::Owned(v)))
      }

      fn visit_seq<V: de::SeqAccess<'a>>(self, mut v: V) -> Result<Value<'a>, V::Error> {
        let mut vec = match v.size_hint() {
          None => Vec::new(),
          Some(s) => Vec::with_capacity(s),
        };
        while let Some(e) = v.next_element()? {
          vec.push(e)
        }
        Ok(Value::Seq(vec))
      }

      fn visit_map<V: de::MapAccess<'a>>(self, mut v: V) -> Result<Value<'a>, V::Error> {
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

pub fn transcode<'de, D, S>(d: D, s: S) -> Result<S::Ok, TranscodeError<S::Error, D::Error>>
where
  D: Deserializer<'de>,
  S: Serializer,
{
  match d.deserialize_any(Visitor(s)) {
    Ok(ser_result) => match ser_result {
      Ok(ser_value) => Ok(ser_value),
      Err(ser_err) => Err(TranscodeError::Ser(ser_err)),
    },
    Err(de_err) => Err(TranscodeError::De(de_err)),
  }
}

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

struct Visitor<S>(S);

macro_rules! __impl_visit_for_transcode {
  ($ty:ty, $visit:ident, $serialize:ident) => {
    fn $visit<E: de::Error>(self, v: $ty) -> Result<Self::Value, E> {
      Ok(self.0.$serialize(v))
    }
  };
}

impl<'de, S> de::Visitor<'de> for Visitor<S>
where
  S: Serializer,
{
  type Value = Result<S::Ok, S::Error>;

  fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    write!(fmt, "any value")
  }

  fn visit_unit<E: de::Error>(self) -> Result<Self::Value, E> {
    Ok(self.0.serialize_unit())
  }

  __impl_visit_for_transcode!(bool, visit_bool, serialize_bool);
  __impl_visit_for_transcode!(i8, visit_i8, serialize_i8);
  __impl_visit_for_transcode!(i16, visit_i16, serialize_i16);
  __impl_visit_for_transcode!(i32, visit_i32, serialize_i32);
  __impl_visit_for_transcode!(i64, visit_i64, serialize_i64);
  __impl_visit_for_transcode!(i128, visit_i128, serialize_i128);
  __impl_visit_for_transcode!(u8, visit_u8, serialize_u8);
  __impl_visit_for_transcode!(u16, visit_u16, serialize_u16);
  __impl_visit_for_transcode!(u32, visit_u32, serialize_u32);
  __impl_visit_for_transcode!(u64, visit_u64, serialize_u64);
  __impl_visit_for_transcode!(u128, visit_u128, serialize_u128);
  __impl_visit_for_transcode!(f32, visit_f32, serialize_f32);
  __impl_visit_for_transcode!(f64, visit_f64, serialize_f64);
  __impl_visit_for_transcode!(char, visit_char, serialize_char);
  __impl_visit_for_transcode!(&str, visit_str, serialize_str);
  __impl_visit_for_transcode!(&[u8], visit_bytes, serialize_bytes);

  fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
  where
    A: de::SeqAccess<'de>,
  {
    let mut ser = match self.0.serialize_seq(seq.size_hint()) {
      Ok(ser) => ser,
      Err(ser_err) => return Ok(Err(ser_err)),
    };

    while let Some(ser_result) = seq.next_element_seed(SeqSeed(&mut ser))? {
      if let Err(ser_err) = ser_result {
        return Ok(Err(ser_err));
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
      Err(ser_err) => return Ok(Err(ser_err)),
    };

    while let Some(ser_result) = map.next_key_seed(KeySeed(&mut ser))? {
      if let Err(ser_err) = ser_result {
        return Ok(Err(ser_err));
      }
      if let Err(ser_err) = map.next_value_seed(ValueSeed(&mut ser))? {
        return Ok(Err(ser_err));
      }
    }

    Ok(ser.end())
  }
}

struct SeqSeed<'a, S: 'a>(&'a mut S);

impl<'a, 'de, S> de::DeserializeSeed<'de> for SeqSeed<'a, S>
where
  S: SerializeSeq,
{
  type Value = Result<(), S::Error>;

  fn deserialize<D>(self, d: D) -> Result<Self::Value, D::Error>
  where
    D: Deserializer<'de>,
  {
    let t = Transcoder::new(d);
    match self.0.serialize_element(&t) {
      Ok(()) => Ok(Ok(())),
      Err(ser_err) => match t.into_error() {
        Some(de_err) => Err(de_err),
        None => Ok(Err(ser_err)),
      },
    }
  }
}

struct KeySeed<'a, S: 'a>(&'a mut S);

impl<'a, 'de, S> de::DeserializeSeed<'de> for KeySeed<'a, S>
where
  S: SerializeMap,
{
  type Value = Result<(), S::Error>;

  fn deserialize<D>(self, d: D) -> Result<Self::Value, D::Error>
  where
    D: Deserializer<'de>,
  {
    let t = Transcoder::new(d);
    match self.0.serialize_key(&t) {
      Ok(()) => Ok(Ok(())),
      Err(ser_err) => match t.into_error() {
        Some(de_err) => Err(de_err),
        None => Ok(Err(ser_err)),
      },
    }
  }
}

struct ValueSeed<'a, S: 'a>(&'a mut S);

impl<'a, 'de, S> de::DeserializeSeed<'de> for ValueSeed<'a, S>
where
  S: SerializeMap,
{
  type Value = Result<(), S::Error>;

  fn deserialize<D>(self, d: D) -> Result<Self::Value, D::Error>
  where
    D: Deserializer<'de>,
  {
    let t = Transcoder::new(d);
    match self.0.serialize_value(&t) {
      Ok(()) => Ok(Ok(())),
      Err(ser_err) => match t.into_error() {
        Some(de_err) => Err(de_err),
        None => Ok(Err(ser_err)),
      },
    }
  }
}

struct Transcoder<'de, D: Deserializer<'de>>(RefCell<TranscoderState<'de, D>>);

enum TranscoderState<'de, D: Deserializer<'de>> {
  New(Option<D>),
  Used(Option<D::Error>),
}

impl<'de, D> Transcoder<'de, D>
where
  D: Deserializer<'de>,
{
  fn new(d: D) -> Transcoder<'de, D> {
    Transcoder(RefCell::new(TranscoderState::New(Some(d))))
  }

  fn into_error(self) -> Option<D::Error> {
    match self.0.into_inner() {
      TranscoderState::New(_) => None,
      TranscoderState::Used(de_result) => de_result,
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
    let de_result = match *self.0.borrow_mut() {
      TranscoderState::Used(_) => panic!("transcoder may only be serialized once"),
      TranscoderState::New(None) => unreachable!(),
      TranscoderState::New(ref mut d) => {
        let d = d.take().unwrap();
        d.deserialize_any(Visitor(s))
      }
    };

    match de_result {
      Ok(ser_result) => {
        *self.0.borrow_mut() = TranscoderState::Used(None);
        ser_result
      }
      Err(de_err) => {
        *self.0.borrow_mut() = TranscoderState::Used(Some(de_err));
        use ser::Error;
        Err(S::Error::custom(
          "deserializer error, must unpack from transcoder",
        ))
      }
    }
  }
}
