//! Automatic detection of data formats based on parser trials.

use std::error;
use std::fmt;
use std::io;

use serde::{de, ser, Deserialize, Serialize, Serializer};

use crate::input;
use crate::{Format, Output};

/// Detects the input format by trying each known format and selecting the first
/// one that works.
pub(crate) fn detect_format(input: &mut input::Handle) -> io::Result<Option<Format>> {
	// As a binary format, we expect MessagePack to be more restrictive than any
	// text format. Detection of MessagePack inputs is limited to collection
	// types; see comments in the implementation for details.
	if crate::msgpack::input_matches(input.borrow_mut())? {
		return Ok(Some(Format::Msgpack));
	}

	// We expect JSON to be more restrictive than other text formats. For
	// example, a "#" comment at the start of a document could be TOML or YAML,
	// but definitely not JSON.
	if crate::json::input_matches(input.borrow_mut())? {
		return Ok(Some(Format::Json));
	}

	// YAML is actually less restrictive than TOML, but we want to try it first
	// since it supports streaming input (which means that detection may require
	// less buffering). Detection of YAML inputs is limited to collection types;
	// see comments in the implementation for details.
	if crate::yaml::input_matches(input.borrow_mut())? {
		return Ok(Some(Format::Yaml));
	}

	// Finally, TOML is the only input format that must fully buffer input
	// before parsing.
	if crate::toml::input_matches(input.borrow_mut())? {
		return Ok(Some(Format::Toml));
	}

	Ok(None)
}

/// The crown jewel of the auto-detection logic: a type that comprehensively
/// throws things away.
struct Discard;

impl Output for Discard {
	fn transcode_from<'de, D, E>(&mut self, de: D) -> crate::Result
	where
		D: de::Deserializer<'de, Error = E>,
		E: de::Error + 'static,
	{
		de::IgnoredAny::deserialize(de)?;
		Ok(())
	}

	fn transcode_value<S>(&mut self, value: S) -> crate::Result
	where
		S: Serialize,
	{
		value.serialize(Discard)?;
		Ok(())
	}

	fn flush(&mut self) -> io::Result<()> {
		Ok(())
	}
}

/// Implements [`Serializer`] methods for [`Discard`] using a custom syntax.
///
/// The syntax combines a function signature (sans return type) in curly braces
/// with a well defined action:
///
/// - `does nothing`: Returns `Ok(())` to ignore primitive values.
/// - `discards $expr`: Serializes `$expr` with the `Discard` serializer to
///   recursively discard complex values like sequences and maps.
/// - `returns Discard`: Returns `Ok(Discard)` to provide access to additional
///   serializer traits.
///
/// This is admittedly a weird macro, but given the high number of methods and
/// traits that a serializer needs to implement, and the extreme consistency in
/// their particular implementations here, I'm willing to believe it ultimately
/// helps more than it hurts.
///
/// This macro is non-hygienic, and not intended for use outside of this module.
macro_rules! xt_detect_impl_discard_methods {
	({ $($decl:tt)* } does nothing; $($rest:tt)*) => {
		fn $($decl)* -> Result<(), Self::Error> {
			Ok(())
		}
		xt_detect_impl_discard_methods! { $($rest)* }
	};
	({ $($decl:tt)* } discards $value:expr; $($rest:tt)*) => {
		fn $($decl)* -> Result<Self::Ok, Self::Error> {
			Serialize::serialize($value, Discard)
		}
		xt_detect_impl_discard_methods! { $($rest)* }
	};
	({ $($decl:tt)* } returns Discard; $($rest:tt)*) => {
		fn $($decl)* -> Result<Discard, Self::Error> {
			Ok(Discard)
		}
		xt_detect_impl_discard_methods! { $($rest)* }
	};
	() => {};
}

impl Serializer for Discard {
	type Ok = ();
	type Error = DiscardError;

	type SerializeSeq = Discard;
	type SerializeTuple = Discard;
	type SerializeTupleStruct = Discard;
	type SerializeTupleVariant = Discard;
	type SerializeMap = Discard;
	type SerializeStruct = Discard;
	type SerializeStructVariant = Discard;

	fn is_human_readable(&self) -> bool {
		false
	}

	xt_detect_impl_discard_methods! {
		{ serialize_unit(self) } does nothing;
		{ serialize_bool(self, _: bool) } does nothing;
		{ serialize_i8(self, _: i8) } does nothing;
		{ serialize_i16(self, _: i16) } does nothing;
		{ serialize_i32(self, _: i32) } does nothing;
		{ serialize_i64(self, _: i64) } does nothing;
		{ serialize_i128(self, _: i128) } does nothing;
		{ serialize_u8(self, _: u8) } does nothing;
		{ serialize_u16(self, _: u16) } does nothing;
		{ serialize_u32(self, _: u32) } does nothing;
		{ serialize_u64(self, _: u64) } does nothing;
		{ serialize_u128(self, _: u128) } does nothing;
		{ serialize_f32(self, _: f32) } does nothing;
		{ serialize_f64(self, _: f64) } does nothing;
		{ serialize_char(self, _: char) } does nothing;
		{ serialize_str(self, _: &str) } does nothing;
		{ serialize_bytes(self, _: &[u8]) } does nothing;
		{ serialize_none(self) } does nothing;
		{ serialize_unit_struct(self, _: &'static str) } does nothing;
		{ serialize_unit_variant(self, _: &'static str, _: u32, _: &'static str) } does nothing;

		{ serialize_some<T: ?Sized + Serialize>(self, value: &T) } discards value;
		{ serialize_newtype_struct<T: ?Sized + Serialize>(self, _: &'static str, value: &T) } discards value;
		{ serialize_newtype_variant<T: ?Sized + Serialize>(self, _: &'static str, _: u32, _: &'static str, value: &T) } discards value;

		{ serialize_seq(self, _: Option<usize>) } returns Discard;
		{ serialize_tuple(self, _: usize) } returns Discard;
		{ serialize_tuple_struct(self, _: &'static str, _: usize) } returns Discard;
		{ serialize_tuple_variant(self, _: &'static str, _: u32, _: &'static str, _: usize) } returns Discard;
		{ serialize_map(self, _: Option<usize>) } returns Discard;
		{ serialize_struct(self, _: &'static str, _: usize) } returns Discard;
		{ serialize_struct_variant(self, _: &'static str, _: u32, _: &'static str, _: usize) } returns Discard;
	}
}

/// Implements additional [`Serializer`] traits for [`Discard`] using
/// [`xt_detect_impl_discard_methods`] syntax.
///
/// This macro is non-hygienic, and not intended for use outside of this module.
macro_rules! xt_detect_impl_discard_traits {
	($($ty:ty { $($body:tt)* })*) => {
		$(impl $ty for Discard {
			type Ok = ();
			type Error = DiscardError;
			xt_detect_impl_discard_methods! { $($body)* }
		})*
	};
}

xt_detect_impl_discard_traits! {
	ser::SerializeSeq {
		{ serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) } discards value;
		{ end(self) } does nothing;
	}

	ser::SerializeTuple {
		{ serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) } discards value;
		{ end(self) } does nothing;
	}

	ser::SerializeTupleStruct {
		{ serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) } discards value;
		{ end(self) } does nothing;
	}

	ser::SerializeTupleVariant {
		{ serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) } discards value;
		{ end(self) } does nothing;
	}

	ser::SerializeMap {
		{ serialize_key<T: ?Sized + Serialize>(&mut self, key: &T) } discards key;
		{ serialize_value<T: ?Sized + Serialize>(&mut self, value: &T) } discards value;
		{ end(self) } does nothing;
	}

	ser::SerializeStruct {
		{ serialize_field<T: ?Sized + Serialize>(&mut self, _: &'static str, value: &T) } discards value;
		{ end(self) } does nothing;
	}

	ser::SerializeStructVariant {
		{ serialize_field<T: ?Sized + Serialize>(&mut self, _: &'static str, value: &T) } discards value;
		{ end(self) } does nothing;
	}
}

/// An error produced by discarding a serializable value.
///
/// This can only be constructed when a value serialized with [`Discard`]
/// invokes [`ser::Error::custom`].
#[derive(Debug)]
struct DiscardError(String);

impl fmt::Display for DiscardError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		fmt::Display::fmt(&self.0, f)
	}
}

impl error::Error for DiscardError {}

impl ser::Error for DiscardError {
	fn custom<T: fmt::Display>(msg: T) -> Self {
		DiscardError(msg.to_string())
	}
}
