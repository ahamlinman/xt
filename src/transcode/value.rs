//! Non-streaming translation between Serde data formats using zero-copy
//! deserialization.

use std::borrow::Cow;
use std::fmt;

use serde::de::{self, Deserialize, Deserializer};
use serde::ser::{Serialize, SerializeMap, Serializer};

/// A deserialized value referencing borrowed data.
///
/// `Value` supports cases where an input format does not provide access to a
/// [`Deserializer`] for direct transcoding, and instead requires deserializing
/// into an in-memory value.
///
/// Unlike most "Serde value" types, `Value` is explicitly optimized for use in
/// transcoding. It uses zero-copy deserialization for byte sequences and
/// strings where possible, which limits the lifetime of the value and the types
/// of inputs it can deserialize from. It represents maps as `Vec`s of key-value
/// pairs, which preserves ordering but does not allow random access to entries.
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
		match self {
			Value::Unit => s.serialize_unit(),
			Value::Bool(b) => s.serialize_bool(*b),
			Value::I8(n) => s.serialize_i8(*n),
			Value::I16(n) => s.serialize_i16(*n),
			Value::I32(n) => s.serialize_i32(*n),
			Value::I64(n) => s.serialize_i64(*n),
			Value::I128(n) => s.serialize_i128(*n),
			Value::U8(n) => s.serialize_u8(*n),
			Value::U16(n) => s.serialize_u16(*n),
			Value::U32(n) => s.serialize_u32(*n),
			Value::U64(n) => s.serialize_u64(*n),
			Value::U128(n) => s.serialize_u128(*n),
			Value::F32(f) => s.serialize_f32(*f),
			Value::F64(f) => s.serialize_f64(*f),
			Value::Char(c) => s.serialize_char(*c),
			Value::String(v) => v.serialize(s),
			Value::Bytes(v) => v.serialize(s),
			Value::Seq(v) => v.serialize(s),
			Value::Map(m) => {
				let mut s = s.serialize_map(Some(m.len()))?;
				for (k, v) in m {
					s.serialize_entry(k, v)?;
				}
				s.end()
			}
		}
	}
}

/// Implements [`de::Visitor`] methods that simply shove values into [`Value`]s.
///
/// This macro is non-hygienic, and not intended for use outside of this module.
macro_rules! xt_transcode_impl_value_visitors {
	($($name:ident($($arg:ident: $ty:ty)?) => $result:expr;)*) => {
		$(fn $name<E: de::Error>(self, $($arg: $ty)?) -> Result<Self::Value, E> {
			Ok($result)
		})*
	};
}

impl<'de: 'a, 'a> Deserialize<'de> for Value<'a> {
	fn deserialize<D>(d: D) -> Result<Value<'a>, D::Error>
	where
		D: Deserializer<'de>,
	{
		struct Visitor;

		impl<'a> de::Visitor<'a> for Visitor {
			type Value = Value<'a>;

			fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
				f.write_str("any supported value")
			}

			xt_transcode_impl_value_visitors! {
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

				visit_borrowed_str(v: &'a str) => Value::String(Cow::Borrowed(v));
				visit_str(v: &str) => Value::String(Cow::Owned(v.to_owned()));
				visit_string(v: String) => Value::String(Cow::Owned(v));

				visit_borrowed_bytes(v: &'a [u8]) => Value::Bytes(Cow::Borrowed(v));
				visit_bytes(v: &[u8]) => Value::Bytes(Cow::Owned(v.to_owned()));
				visit_byte_buf(v: Vec<u8>) => Value::Bytes(Cow::Owned(v));
			}

			fn visit_seq<A: de::SeqAccess<'a>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
				let mut vec = Vec::with_capacity(seq.size_hint().unwrap_or(0));
				while let Some(e) = seq.next_element()? {
					vec.push(e);
				}
				Ok(Value::Seq(vec))
			}

			fn visit_map<A: de::MapAccess<'a>>(self, mut map: A) -> Result<Self::Value, A::Error> {
				let mut vec = Vec::with_capacity(map.size_hint().unwrap_or(0));
				while let Some(entry) = map.next_entry()? {
					vec.push(entry);
				}
				Ok(Value::Map(vec))
			}
		}

		d.deserialize_any(Visitor)
	}
}
