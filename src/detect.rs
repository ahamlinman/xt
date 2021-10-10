use std::error::Error;
use std::fmt;
use std::io;

use serde::{de, ser, Deserialize, Serialize, Serializer};

use crate::{transcode_input, Format, InputHandle, Output};

pub(crate) fn detect_format(mut input: InputHandle) -> io::Result<Option<Format>> {
  // JSON comes first as it is relatively restrictive compared to the other
  // formats. For example, a "#" comment at the start of a doc could be TOML or
  // YAML, but definitely not JSON, so we can abort parsing fairly early.
  //
  // TOML comes next as it is less restrictive than JSON, but still more
  // restrictive than YAML. In fact, TOML documents that don't start with a
  // table can be parsed as a plain style flow scalar in YAML, i.e. as a giant
  // string. Cargo.lock is a great example of this, if you're curious.
  for from in [Format::Json, Format::Toml] {
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
  use rmp::Marker::{self, *};
  if matches!(
    input.try_as_buffer()?.get(0).map(|b| Marker::from_u8(*b)),
    Some(FixArray(_) | Array16 | Array32 | FixMap(_) | Map16 | Map32)
  ) {
    if let Ok(_) = transcode_input(input.try_clone()?, Format::Msgpack, Discard) {
      return Ok(Some(Format::Msgpack));
    }
  }

  // Finally, YAML is our traditional fallback format. Yes, we still get the
  // giant string behavior described above for arbitrary text documents, but it
  // is how jyt has worked for a long time and it's not like the behavior is
  // actively harmful. We do try to defer the YAML check for as long as we can,
  // since it will try to detect and re-encode UTF-16 and UTF-32 input, which
  // might be expensive. In particular, we do it after the MessagePack check
  // since some valid MessagePack values would be false matches under the YAML
  // 1.2 encoding detection algorithm. For example, a MessagePack fixarray with
  // the integer 0 as its first value encodes as 0x9_ 0x00, which matches one of
  // the byte patterns for UTF-16-LE YAML input.
  if let Ok(_) = transcode_input(input, Format::Yaml, Discard) {
    return Ok(Some(Format::Yaml));
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
      Err(err) => Err(err.into()),
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
/// [`Discard`], using terms of the following form:
///
/// ```
/// // Returns the result of serializing `$expr` with the `Discard` serializer
/// ([function signature]) discards $expr;
///
/// // Returns `Ok(Discard)` as a `Result<Discard, Self::Error>`
/// ([function signature]) returns Discard;
///
/// // Returns `Ok(())`
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

/// Implements the additional traits required of a [`serde::Serializer`] on
/// [`Discard`], using our special macro syntax for serializer methods.
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
