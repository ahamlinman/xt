use std::error::Error;
use std::io::Write;

use serde::Deserialize;

pub struct Output<W: Write> {
  w: W,
  used: bool,
}

impl<W: Write> Output<W> {
  pub fn new(w: W) -> Output<W> {
    Output { w: w, used: false }
  }
}

impl<W: Write> crate::Output for Output<W> {
  fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<(), Box<dyn Error>>
  where
    D: serde::de::Deserializer<'de, Error = E>,
    E: serde::de::Error + 'static,
  {
    self.used = match self.used {
      false => true,
      true => Err("TOML does not support multi-document output")?,
    };

    // TOML requires that all non-table values appear before any tables at a
    // given "level." Since we can't enforce this for all input types, we buffer
    // the inputs into a toml::Value, which will serialize them back out in the
    // necessary order.
    let value = ::toml::Value::deserialize(de)?;

    // From the spec: "TOML is designed to map unambiguously to a hash table."
    // Without this check, the other input types could produce something like a
    // boolean or array that we would attempt to dump the TOML representation of
    // without a second thought. The toml crate can even produce invalid TOML
    // for some of these representations, such as dumping each element of an
    // array of tables with an empty name, i.e. with a "[[]]" header.
    if !value.is_table() {
      Err("root of TOML output must be a table")?;
    }

    // As of this writing, the toml crate can't output directly to a writer.
    let output_buf = ::toml::to_string_pretty(&value)?;
    self.w.write_all(output_buf.as_bytes())?;
    Ok(())
  }
}
