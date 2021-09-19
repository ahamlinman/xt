use std::borrow::Cow;
use std::error::Error;
use std::fmt;
use std::io::{BufReader, Write};

use serde::{Deserialize, Serialize};

use crate::{Input, InputRef, TranscodeFrom};

pub(crate) fn transcode<T>(input: InputRef, mut output: T) -> Result<(), Box<dyn Error>>
where
  T: TranscodeFrom,
{
  // The two implementations here were chosen based on some simple performance
  // testing with various inputs. Deserializer::from_slice generally performs
  // better than Deserialize::from_reader (even when reading from a slice),
  // however the .end method in slice mode is extremely slow. Creating a true
  // iterator requires deserializing into a value, so jyt has a special, faster
  // alternative to serde_json::Value that can capture borrowed data. In
  // contrast, when streaming input from a reader, direct transcoding is much
  // faster. My unproven guess is that it has to allocate Strings for
  // BorrowedValue, where with direct transcoding it can forward buffered &str
  // slices straight to the serializer.
  match input.into() {
    Input::Buffered(buf) => {
      let de = serde_json::Deserializer::from_slice(&buf);
      for value in de.into_iter::<BorrowedValue>() {
        output.transcode_value(value?)?;
      }
    }
    Input::Unbuffered(r) => {
      let mut de = serde_json::Deserializer::from_reader(BufReader::new(r));
      while let Err(_) = de.end() {
        output.transcode_from(&mut de)?;
      }
    }
  }
  Ok(())
}

pub struct Output<W: Write>(W);

impl<W: Write> Output<W> {
  pub fn new(w: W) -> Output<W> {
    Output(w)
  }
}

impl<W: Write> TranscodeFrom for Output<W> {
  fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<(), Box<dyn Error>>
  where
    D: serde::de::Deserializer<'de, Error = E>,
    E: serde::de::Error + 'static,
  {
    let mut ser = serde_json::Serializer::new(&mut self.0);
    serde_transcode::transcode(de, &mut ser)?;
    writeln!(&mut self.0, "")?;
    Ok(())
  }

  fn transcode_value<S>(&mut self, value: S) -> Result<(), Box<dyn Error>>
  where
    S: serde::ser::Serialize,
  {
    serde_json::to_writer(&mut self.0, &value)?;
    writeln!(&mut self.0, "")?;
    Ok(())
  }
}

/// Represents any valid JSON value using data borrowed from a JSON
/// deserializer, with map ordering preserved.
enum BorrowedValue<'de> {
  Null,
  Bool(bool),
  Number(serde_json::Number),
  String(CowString<'de>),
  Array(Vec<BorrowedValue<'de>>),
  Object(Vec<(CowString<'de>, BorrowedValue<'de>)>),
}

impl Serialize for BorrowedValue<'_> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    match self {
      BorrowedValue::Null => serializer.serialize_unit(),
      BorrowedValue::Bool(b) => serializer.serialize_bool(*b),
      BorrowedValue::Number(n) => n.serialize(serializer),
      BorrowedValue::String(s) => s.serialize(serializer),
      BorrowedValue::Array(a) => a.serialize(serializer),
      BorrowedValue::Object(m) => {
        use serde::ser::SerializeMap;
        let mut map = serializer.serialize_map(Some(m.len()))?;
        for (k, v) in m {
          map.serialize_entry(k, v)?;
        }
        map.end()
      }
    }
  }
}

impl<'de> Deserialize<'de> for BorrowedValue<'de> {
  fn deserialize<D>(deserializer: D) -> Result<BorrowedValue<'de>, D::Error>
  where
    D: serde::de::Deserializer<'de>,
  {
    struct Visitor;

    impl<'de> serde::de::Visitor<'de> for Visitor {
      type Value = BorrowedValue<'de>;

      fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("any valid JSON value")
      }

      fn visit_bool<E>(self, value: bool) -> Result<BorrowedValue<'de>, E> {
        Ok(BorrowedValue::Bool(value))
      }

      fn visit_i64<E>(self, value: i64) -> Result<BorrowedValue<'de>, E> {
        Ok(BorrowedValue::Number(value.into()))
      }

      fn visit_u64<E>(self, value: u64) -> Result<BorrowedValue<'de>, E> {
        Ok(BorrowedValue::Number(value.into()))
      }

      fn visit_f64<E>(self, value: f64) -> Result<BorrowedValue<'de>, E> {
        Ok(serde_json::Number::from_f64(value).map_or(BorrowedValue::Null, BorrowedValue::Number))
      }

      fn visit_str<E>(self, value: &str) -> Result<BorrowedValue<'de>, E> {
        Ok(BorrowedValue::String(CowString(Cow::Owned(value.into()))))
      }

      fn visit_borrowed_str<E>(self, value: &'de str) -> Result<BorrowedValue<'de>, E> {
        Ok(BorrowedValue::String(CowString(Cow::Borrowed(value))))
      }

      fn visit_string<E>(self, value: String) -> Result<BorrowedValue<'de>, E> {
        Ok(BorrowedValue::String(CowString(Cow::Owned(value))))
      }

      fn visit_unit<E>(self) -> Result<BorrowedValue<'de>, E> {
        Ok(BorrowedValue::Null)
      }

      fn visit_none<E>(self) -> Result<BorrowedValue<'de>, E> {
        Ok(BorrowedValue::Null)
      }

      fn visit_some<D>(self, deserializer: D) -> Result<BorrowedValue<'de>, D::Error>
      where
        D: serde::de::Deserializer<'de>,
      {
        Deserialize::deserialize(deserializer)
      }

      fn visit_seq<V>(self, mut visitor: V) -> Result<BorrowedValue<'de>, V::Error>
      where
        V: serde::de::SeqAccess<'de>,
      {
        let mut vec = match visitor.size_hint() {
          None => Vec::new(),
          Some(s) => Vec::with_capacity(s),
        };
        while let Some(e) = visitor.next_element()? {
          vec.push(e)
        }
        Ok(BorrowedValue::Array(vec))
      }

      fn visit_map<V>(self, mut visitor: V) -> Result<BorrowedValue<'de>, V::Error>
      where
        V: serde::de::MapAccess<'de>,
      {
        let mut vec = match visitor.size_hint() {
          None => Vec::new(),
          Some(s) => Vec::with_capacity(s),
        };
        while let Some(entry) = visitor.next_entry()? {
          vec.push(entry)
        }
        Ok(BorrowedValue::Object(vec))
      }
    }

    deserializer.deserialize_any(Visitor)
  }
}

/// Represents a string that may be deserialized using borrowed data. See
/// https://github.com/serde-rs/serde/issues/1852.
struct CowString<'de>(Cow<'de, str>);

impl Serialize for CowString<'_> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    serializer.serialize_str(&self.0)
  }
}

impl<'de> Deserialize<'de> for CowString<'de> {
  fn deserialize<D>(deserializer: D) -> Result<CowString<'de>, D::Error>
  where
    D: serde::de::Deserializer<'de>,
  {
    struct Visitor;

    impl<'de> serde::de::Visitor<'de> for Visitor {
      type Value = CowString<'de>;

      fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("any valid string")
      }

      fn visit_str<E>(self, value: &str) -> Result<CowString<'de>, E> {
        Ok(CowString(Cow::Owned(value.into())))
      }

      fn visit_borrowed_str<E>(self, value: &'de str) -> Result<CowString<'de>, E> {
        Ok(CowString(Cow::Borrowed(value.into())))
      }

      fn visit_string<E>(self, value: String) -> Result<CowString<'de>, E> {
        Ok(CowString(Cow::Owned(value)))
      }
    }

    deserializer.deserialize_str(Visitor)
  }
}
