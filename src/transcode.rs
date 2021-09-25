use std::borrow::Cow;
use std::fmt;

use serde::{de, Deserialize, Deserializer, Serialize, Serializer};

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
        use serde::ser::SerializeMap;
        let mut map = s.serialize_map(Some(m.len()))?;
        for (k, v) in m {
          map.serialize_entry(k, v)?;
        }
        map.end()
      }
    }
  }
}

macro_rules! __impl_visit_scalar {
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

      __impl_visit_scalar!(bool, visit_bool, Value::Bool);
      __impl_visit_scalar!(i8, visit_i8, Value::I8);
      __impl_visit_scalar!(i16, visit_i16, Value::I16);
      __impl_visit_scalar!(i32, visit_i32, Value::I32);
      __impl_visit_scalar!(i64, visit_i64, Value::I64);
      __impl_visit_scalar!(i128, visit_i128, Value::I128);
      __impl_visit_scalar!(u8, visit_u8, Value::U8);
      __impl_visit_scalar!(u16, visit_u16, Value::U16);
      __impl_visit_scalar!(u32, visit_u32, Value::U32);
      __impl_visit_scalar!(u64, visit_u64, Value::U64);
      __impl_visit_scalar!(u128, visit_u128, Value::U128);
      __impl_visit_scalar!(f32, visit_f32, Value::F32);
      __impl_visit_scalar!(f64, visit_f64, Value::F64);
      __impl_visit_scalar!(char, visit_char, Value::Char);

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
