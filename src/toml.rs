use std::error::Error;
use std::io::Write;
use std::str;

use serde::Deserialize;

use crate::InputRef;

pub(crate) fn transcode<O>(mut input: InputRef, mut output: O) -> Result<(), Box<dyn Error>>
where
  O: crate::Output,
{
  // Since TOML (thankfully) requires UTF-8 encoding, and doesn't support
  // multiple documents in a single stream, the toml crate only takes input as a
  // &str. This is the only format where we have no choice but to slurp all
  // input into memory, but honestly that's fine.
  let input_str = str::from_utf8(input.try_buffer()?)?;
  let mut de = ::toml::Deserializer::new(input_str);
  output.transcode_from(&mut de)
}

pub(crate) struct Output<W: Write> {
  w: W,
  used: bool,
}

impl<W: Write> Output<W> {
  pub fn new(w: W) -> Output<W> {
    Output { w: w, used: false }
  }
}

impl<W: Write> Output<W> {
  fn use_once(&mut self) -> Result<(), &'static str> {
    self.used = match self.used {
      false => true,
      true => return Err("TOML does not support multi-document output"),
    };
    Ok(())
  }
}

impl<W: Write> crate::Output for Output<W> {
  fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<(), Box<dyn Error>>
  where
    D: serde::de::Deserializer<'de, Error = E>,
    E: serde::de::Error + 'static,
  {
    // TOML is pretty unique among jyt's supported output formats, and requires
    // several special considerations.

    // First, TOML has no concept of multiple documents in a single stream.
    // Since we can't know the number of input documents in advance, we just
    // have to fail if we see a second one.
    self.use_once()?;

    // Second, it's not safe to transcode the input directly, as the ordering of
    // keys and values in maps might not be TOML-compatible. TOML requires that
    // non-table values appear before any tables at a given nesting level, which
    // toml::Value knows how to take care of. We can activate the toml crate's
    // "preserve_order" feature to keep as much of the original ordering as
    // possible.
    let value = ::toml::Value::deserialize(de)?;

    // Finally, "TOML is designed to map unambiguously to a hash table" (quoted
    // from the spec). jyt's other input formats can produce something like a
    // boolean or array at the top level, which we would dump into an invalid
    // TOML document if we're not careful. For example, a top-level array could
    // get dumped as an array of tables with an empty name, i.e. with "[[]]"
    // headers.
    if !value.is_table() {
      return Err("root of TOML output must be a table".into());
    }

    // As of this writing, the toml crate can't output directly to a writer.
    let output_buf = ::toml::to_string_pretty(&value)?;
    self.w.write_all(output_buf.as_bytes())?;
    Ok(())
  }

  fn transcode_value<S>(&mut self, value: S) -> Result<(), Box<dyn Error>>
  where
    S: serde::ser::Serialize,
  {
    self.use_once()?;
    let output_buf = ::toml::to_string_pretty(&value)?;
    self.w.write_all(output_buf.as_bytes())?;
    Ok(())
  }
}
