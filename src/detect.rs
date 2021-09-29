use std::error::Error;
use std::fmt;
use std::io;

use serde::{de, ser, Deserialize, Serializer};

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
  match input.try_buffer()?.get(0).map(|b| rmp::Marker::from_u8(*b)) {
    Some(FixArray(_) | Array16 | Array32 | FixMap(_) | Map16 | Map32) => {
      if let Ok(_) = transcode_input(input, Format::Msgpack, Discard) {
        return Ok(Some(Format::Msgpack));
      }
    }
    _ => {}
  }

  Ok(None)
}

/// Discards input in a wide variety of fun and exciting ways.
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
    S: ser::Serialize,
  {
    value.serialize(Discard)?;
    Ok(())
  }
}

/// Implements the methods of a [`serde::Serializer`] that do nothing more than
/// shove the result of some expression into an `Ok` variant, using terms of the
/// following form:
///
/// ```
/// (<function signature>): <type> => <value>;
/// ```
///
/// Optionally, a term can consist solely of the (parenthesized) function
/// signature, and the resulting function will return `Ok(())`.
///
/// This macro is intentionally non-hygienic, and not intended for use outside
/// of this module.
macro_rules! local_impl_infallible_serializer_methods {
  () => {};
  (($($decl:tt)*): $ty:ty => $result:expr; $($rest:tt)*) => {
    fn $($decl)* -> Result<$ty, Self::Error> {
      Ok($result)
    }
    local_impl_infallible_serializer_methods! { $($rest)* }
  };
  (($($decl:tt)*); $($rest:tt)*) => {
    fn $($decl)* -> Result<(), Self::Error> {
      Ok(())
    }
    local_impl_infallible_serializer_methods! { $($rest)* }
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

  local_impl_infallible_serializer_methods! {
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
    (serialize_some<T: ?Sized>(self, _: &T));

    (serialize_unit_struct(self, _: &'static str));
    (serialize_unit_variant(self, _: &'static str, _: u32, _: &'static str));

    (serialize_newtype_struct<T: ?Sized>(self, _: &'static str, _: &T));
    (serialize_newtype_variant<T: ?Sized>(self, _: &'static str, _: u32, _: &'static str, _: &T));

    (serialize_seq(self, _: Option<usize>)):
      Self::SerializeSeq => Discard;

    (serialize_tuple(self, _: usize)):
      Self::SerializeTuple => Discard;

    (serialize_tuple_struct(self, _: &'static str, _: usize)):
      Self::SerializeTupleStruct => Discard;

    (serialize_tuple_variant(self, _: &'static str, _: u32, _: &'static str, _: usize)):
      Self::SerializeTupleVariant => Discard;

    (serialize_map(self, _: Option<usize>)):
      Self::SerializeMap => Discard;

    (serialize_struct(self, _: &'static str, _: usize)):
      Self::SerializeStruct => Discard;

    (serialize_struct_variant(self, _: &'static str, _: u32, _: &'static str, _: usize)):
      Self::SerializeStructVariant => Discard;
  }
}

impl ser::SerializeSeq for Discard {
  type Ok = ();
  type Error = DiscardError;

  local_impl_infallible_serializer_methods! {
    (serialize_element<T: ?Sized>(&mut self, _: &T));
    (end(self));
  }
}

impl ser::SerializeTuple for Discard {
  type Ok = ();
  type Error = DiscardError;

  local_impl_infallible_serializer_methods! {
    (serialize_element<T: ?Sized>(&mut self, _: &T));
    (end(self));
  }
}

impl ser::SerializeTupleStruct for Discard {
  type Ok = ();
  type Error = DiscardError;

  local_impl_infallible_serializer_methods! {
    (serialize_field<T: ?Sized>(&mut self, _: &T));
    (end(self));
  }
}

impl ser::SerializeTupleVariant for Discard {
  type Ok = ();
  type Error = DiscardError;

  local_impl_infallible_serializer_methods! {
    (serialize_field<T: ?Sized>(&mut self, _: &T));
    (end(self));
  }
}

impl ser::SerializeMap for Discard {
  type Ok = ();
  type Error = DiscardError;

  local_impl_infallible_serializer_methods! {
    (serialize_key<T: ?Sized>(&mut self, _: &T));
    (serialize_value<T: ?Sized>(&mut self, _: &T));
    (end(self));
  }
}

impl ser::SerializeStruct for Discard {
  type Ok = ();
  type Error = DiscardError;

  local_impl_infallible_serializer_methods! {
    (serialize_field<T: ?Sized>(&mut self, _: &'static str, _: &T));
    (end(self));
  }
}

impl ser::SerializeStructVariant for Discard {
  type Ok = ();
  type Error = DiscardError;

  local_impl_infallible_serializer_methods! {
    (serialize_field<T: ?Sized>(&mut self, _: &'static str, _: &T));
    (end(self));
  }
}

#[derive(Debug)]
enum DiscardError {}

impl fmt::Display for DiscardError {
  fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
    unreachable!();
  }
}

impl Error for DiscardError {}

impl ser::Error for DiscardError {
  fn custom<T: fmt::Display>(_: T) -> Self {
    unreachable!();
  }
}
