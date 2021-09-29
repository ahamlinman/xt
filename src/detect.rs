use std::error::Error;
use std::fmt;
use std::io;

use serde::{de, ser, Deserialize, Serialize, Serializer};

use crate::{transcode_input, Format, InputRef, Output};

pub(crate) fn detect_format(mut input: InputRef) -> io::Result<Option<Format>> {
  // JSON comes first as it is relatively restrictive compared to the other
  // formats. For example, a "#" comment at the start of a doc could be TOML or
  // YAML, but definitely not JSON, so we can abort parsing fairly early.
  //
  // TOML comes next for a surprising reason… the YAML parser will sometimes
  // accept a TOML file, parsing its contents as a giant string! I'm not sure
  // I'll ever want to understand YAML well enough to explain this. Cargo.lock
  // generally exhibits the behavior, if you're curious.
  //
  // YAML rounds out the text-based formats to help match the behavior of older
  // versions of jyt, which always used YAML as the fallback for unknown input
  // types (I guess if you really do want the giant string behavior).
  for from in [Format::Json, Format::Toml, Format::Yaml] {
    if let Ok(_) = transcode_input(input.try_clone()?, from, Discard) {
      return Ok(Some(from));
    }
  }

  // In MessagePack, any byte below 0x80 represents a literal unsigned integer.
  // That means any ASCII text input is effectively a valid multi-document
  // MessagePack stream, where every "document" is practically meaningless. To
  // prevent these kinds of weird matches, we only attempt to auto-detect
  // MessagePack when the first byte of input indicates that the next value will
  // be a map or array. Arbitrary non-ASCII input that happens to match one of
  // these markers (e.g. certain UTF-8 multibyte sequences) is extremely
  // unlikely to be a valid sequence of MessagePack values.
  use rmp::Marker::*;
  if matches!(
    input.try_buffer()?.get(0).map(|b| rmp::Marker::from_u8(*b)),
    Some(FixArray(_) | Array16 | Array32 | FixMap(_) | Map16 | Map32)
  ) {
    if let Ok(_) = transcode_input(input, Format::Msgpack, Discard) {
      return Ok(Some(Format::Msgpack));
    }
  }

  Ok(None)
}

/// Throws stuff away in a wide variety of fun and exciting ways. Truly the
/// crown jewel of the auto-detection logic.
struct Discard;

impl Output for Discard {
  fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<(), Box<dyn Error>>
  where
    D: de::Deserializer<'de, Error = E>,
    E: de::Error + 'static,
  {
    match de::IgnoredAny::deserialize(de) {
      Ok(_) => Ok(()),
      Err(err) => Err(err)?,
    }
  }

  fn transcode_value<S>(&mut self, value: S) -> Result<(), Box<dyn Error>>
  where
    S: Serialize,
  {
    value.serialize(Discard)?;
    Ok(())
  }
}

/// Implements the methods of a [`serde::Serializer`] that are useful for
/// `Discard`, using terms of the following form:
///
/// ```
/// // Returns the result of serializing `$expr` with the `Discard` serializer
/// ([function signature]) discards $expr;
///
/// // Returns `Ok(Discard)` as a `Result<Discard, Self::Error>`
/// ([function signature]) returns Discard;
///
/// // Returns Ok(())
/// ([function signature]);
/// ```
///
/// This macro is non-hygienic, and not intended for use outside of this module.
macro_rules! local_impl_discard_serializer_methods {
  () => {};
  (($($decl:tt)*) discards $value:expr; $($rest:tt)*) => {
    fn $($decl)* -> Result<Self::Ok, Self::Error> {
      Serialize::serialize($value, Discard)
    }
    local_impl_discard_serializer_methods! { $($rest)* }
  };
  (($($decl:tt)*) returns Discard; $($rest:tt)*) => {
    fn $($decl)* -> Result<Discard, Self::Error> {
      Ok(Discard)
    }
    local_impl_discard_serializer_methods! { $($rest)* }
  };
  (($($decl:tt)*); $($rest:tt)*) => {
    fn $($decl)* -> Result<(), Self::Error> {
      Ok(())
    }
    local_impl_discard_serializer_methods! { $($rest)* }
  };
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

  local_impl_discard_serializer_methods! {
    (serialize_unit(self));
    (serialize_bool(self, _: bool));
    (serialize_i8(self, _: i8));
    (serialize_i16(self, _: i16));
    (serialize_i32(self, _: i32));
    (serialize_i64(self, _: i64));
    (serialize_i128(self, _: i128));
    (serialize_u8(self, _: u8));
    (serialize_u16(self, _: u16));
    (serialize_u32(self, _: u32));
    (serialize_u64(self, _: u64));
    (serialize_u128(self, _: u128));
    (serialize_f32(self, _: f32));
    (serialize_f64(self, _: f64));
    (serialize_char(self, _: char));
    (serialize_str(self, _: &str));
    (serialize_bytes(self, _: &[u8]));
    (serialize_none(self));
    (serialize_unit_struct(self, _: &'static str));
    (serialize_unit_variant(self, _: &'static str, _: u32, _: &'static str));

    (serialize_some<T: ?Sized + Serialize>(self, value: &T))
      discards value;
    (serialize_newtype_struct<T: ?Sized + Serialize>(self, _: &'static str, value: &T))
      discards value;
    (serialize_newtype_variant<T: ?Sized + Serialize>(self, _: &'static str, _: u32, _: &'static str, value: &T))
      discards value;

    (serialize_seq(self, _: Option<usize>))
      returns Discard;
    (serialize_tuple(self, _: usize))
      returns Discard;
    (serialize_tuple_struct(self, _: &'static str, _: usize))
      returns Discard;
    (serialize_tuple_variant(self, _: &'static str, _: u32, _: &'static str, _: usize))
      returns Discard;
    (serialize_map(self, _: Option<usize>))
      returns Discard;
    (serialize_struct(self, _: &'static str, _: usize))
      returns Discard;
    (serialize_struct_variant(self, _: &'static str, _: u32, _: &'static str, _: usize))
      returns Discard;
  }
}

/// Implements the additional traits required of a [`serde::Serializer`] on our
/// `Discard` type, using our special macro syntax for serializer methods.
///
/// This macro is non-hygienic, and not intended for use outside of this module.
macro_rules! local_impl_discard_serializer_traits {
  () => {};
  ($ty:ty { $($body:tt)* }; $($rest:tt)*) => {
    impl $ty for Discard {
      type Ok = ();
      type Error = DiscardError;

      local_impl_discard_serializer_methods! { $($body)* }
    }

    local_impl_discard_serializer_traits! { $($rest)* }
  };
}

local_impl_discard_serializer_traits! {
  ser::SerializeSeq {
    (serialize_element<T: ?Sized + Serialize>(&mut self, value: &T))
      discards value;
    (end(self));
  };

  ser::SerializeTuple {
    (serialize_element<T: ?Sized + Serialize>(&mut self, value: &T))
      discards value;
    (end(self));
  };

  ser::SerializeTupleStruct {
    (serialize_field<T: ?Sized + Serialize>(&mut self, value: &T))
      discards value;
    (end(self));
  };

  ser::SerializeTupleVariant {
    (serialize_field<T: ?Sized + Serialize>(&mut self, value: &T))
      discards value;
    (end(self));
  };

  ser::SerializeMap {
    (serialize_key<T: ?Sized + Serialize>(&mut self, key: &T))
      discards key;
    (serialize_value<T: ?Sized + Serialize>(&mut self, value: &T))
      discards value;
    (end(self));
  };

  ser::SerializeStruct {
    (serialize_field<T: ?Sized + Serialize>(&mut self, _: &'static str, value: &T))
      discards value;
    (end(self));
  };

  ser::SerializeStructVariant {
    (serialize_field<T: ?Sized + Serialize>(&mut self, _: &'static str, value: &T))
      discards value;
    (end(self));
  };
}

/// An error type for the [`Discard`] type's mostly infallible implementation of
/// [`serde::Serializer`]. It can only be constructed when the value being
/// serialized invokes the `custom` function.
#[derive(Debug)]
struct DiscardError(String);

impl fmt::Display for DiscardError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    fmt::Display::fmt(&self.0, f)
  }
}

impl Error for DiscardError {}

impl ser::Error for DiscardError {
  fn custom<T: fmt::Display>(msg: T) -> Self {
    DiscardError(msg.to_string())
  }
}
