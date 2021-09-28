use std::error::Error;
use std::fmt;
use std::io;

use serde::{ser, Deserialize, Serializer};

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

struct Discard;

impl Output for Discard {
  fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<(), Box<dyn Error>>
  where
    D: serde::de::Deserializer<'de, Error = E>,
    E: serde::de::Error + 'static,
  {
    match serde::de::IgnoredAny::deserialize(de) {
      Ok(_) => Ok(()),
      Err(err) => Err(err)?,
    }
  }

  fn transcode_value<S>(&mut self, value: S) -> Result<(), Box<dyn Error>>
  where
    S: serde::ser::Serialize,
  {
    value.serialize(Discard)?;
    Ok(())
  }
}

macro_rules! impl_discard_serializer_method {
  ($name:ident) => {
    fn $name(self) -> Result<Self::Ok, Self::Error> {
      Ok(())
    }
  };
  ($ty:ty, $name:ident) => {
    fn $name(self, _: $ty) -> Result<Self::Ok, Self::Error> {
      Ok(())
    }
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

  impl_discard_serializer_method!(serialize_unit);
  impl_discard_serializer_method!(serialize_none);
  impl_discard_serializer_method!(bool, serialize_bool);
  impl_discard_serializer_method!(i8, serialize_i8);
  impl_discard_serializer_method!(i16, serialize_i16);
  impl_discard_serializer_method!(i32, serialize_i32);
  impl_discard_serializer_method!(i64, serialize_i64);
  impl_discard_serializer_method!(i128, serialize_i128);
  impl_discard_serializer_method!(u8, serialize_u8);
  impl_discard_serializer_method!(u16, serialize_u16);
  impl_discard_serializer_method!(u32, serialize_u32);
  impl_discard_serializer_method!(u64, serialize_u64);
  impl_discard_serializer_method!(u128, serialize_u128);
  impl_discard_serializer_method!(f32, serialize_f32);
  impl_discard_serializer_method!(f64, serialize_f64);
  impl_discard_serializer_method!(char, serialize_char);
  impl_discard_serializer_method!(&str, serialize_str);
  impl_discard_serializer_method!(&[u8], serialize_bytes);

  fn serialize_some<T: ?Sized>(self, _: &T) -> Result<Self::Ok, Self::Error> {
    Ok(())
  }

  fn serialize_unit_struct(self, _: &'static str) -> Result<Self::Ok, Self::Error> {
    Ok(())
  }

  fn serialize_unit_variant(
    self,
    _: &'static str,
    _: u32,
    _: &'static str,
  ) -> Result<Self::Ok, Self::Error> {
    Ok(())
  }

  fn serialize_newtype_struct<T: ?Sized>(
    self,
    _: &'static str,
    _: &T,
  ) -> Result<Self::Ok, Self::Error> {
    Ok(())
  }

  fn serialize_newtype_variant<T: ?Sized>(
    self,
    _: &'static str,
    _: u32,
    _: &'static str,
    _: &T,
  ) -> Result<Self::Ok, Self::Error> {
    Ok(())
  }

  fn serialize_seq(self, _: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
    Ok(Discard)
  }

  fn serialize_tuple(self, _: usize) -> Result<Self::SerializeTuple, Self::Error> {
    Ok(Discard)
  }

  fn serialize_tuple_struct(
    self,
    _: &'static str,
    _: usize,
  ) -> Result<Self::SerializeTupleStruct, Self::Error> {
    Ok(Discard)
  }

  fn serialize_tuple_variant(
    self,
    _: &'static str,
    _: u32,
    _: &'static str,
    _: usize,
  ) -> Result<Self::SerializeTupleVariant, Self::Error> {
    Ok(Discard)
  }

  fn serialize_map(self, _: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
    Ok(Discard)
  }

  fn serialize_struct(
    self,
    _: &'static str,
    _: usize,
  ) -> Result<Self::SerializeStruct, Self::Error> {
    Ok(Discard)
  }

  fn serialize_struct_variant(
    self,
    _: &'static str,
    _: u32,
    _: &'static str,
    _: usize,
  ) -> Result<Self::SerializeStructVariant, Self::Error> {
    Ok(Discard)
  }
}

impl ser::SerializeSeq for Discard {
  type Ok = ();
  type Error = DiscardError;

  fn serialize_element<T: ?Sized>(&mut self, _: &T) -> Result<Self::Ok, Self::Error> {
    Ok(())
  }

  fn end(self) -> Result<Self::Ok, Self::Error> {
    Ok(())
  }
}

impl ser::SerializeTuple for Discard {
  type Ok = ();
  type Error = DiscardError;

  fn serialize_element<T: ?Sized>(&mut self, _: &T) -> Result<Self::Ok, Self::Error> {
    Ok(())
  }

  fn end(self) -> Result<Self::Ok, Self::Error> {
    Ok(())
  }
}

impl ser::SerializeTupleStruct for Discard {
  type Ok = ();
  type Error = DiscardError;

  fn serialize_field<T: ?Sized>(&mut self, _: &T) -> Result<Self::Ok, Self::Error> {
    Ok(())
  }

  fn end(self) -> Result<Self::Ok, Self::Error> {
    Ok(())
  }
}

impl ser::SerializeTupleVariant for Discard {
  type Ok = ();
  type Error = DiscardError;

  fn serialize_field<T: ?Sized>(&mut self, _: &T) -> Result<Self::Ok, Self::Error> {
    Ok(())
  }

  fn end(self) -> Result<Self::Ok, Self::Error> {
    Ok(())
  }
}

impl ser::SerializeMap for Discard {
  type Ok = ();
  type Error = DiscardError;

  fn serialize_key<T: ?Sized>(&mut self, _: &T) -> Result<(), Self::Error> {
    Ok(())
  }

  fn serialize_value<T: ?Sized>(&mut self, _: &T) -> Result<(), Self::Error> {
    Ok(())
  }

  fn end(self) -> Result<Self::Ok, Self::Error> {
    Ok(())
  }
}

impl ser::SerializeStruct for Discard {
  type Ok = ();
  type Error = DiscardError;

  fn serialize_field<T: ?Sized>(
    &mut self,
    _: &'static str,
    _: &T,
  ) -> Result<Self::Ok, Self::Error> {
    Ok(())
  }

  fn end(self) -> Result<Self::Ok, Self::Error> {
    Ok(())
  }
}

impl ser::SerializeStructVariant for Discard {
  type Ok = ();
  type Error = DiscardError;

  fn serialize_field<T: ?Sized>(
    &mut self,
    _: &'static str,
    _: &T,
  ) -> Result<Self::Ok, Self::Error> {
    Ok(())
  }

  fn end(self) -> Result<Self::Ok, Self::Error> {
    Ok(())
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
