use std::error::Error;
use std::io::{BufReader, Write};

use crate::{transcode, Input, InputRef};

pub(crate) fn transcode<O>(input: InputRef, mut output: O) -> Result<(), Box<dyn Error>>
where
  O: crate::Output,
{
  match input.into() {
    Input::Buffer(buf) => {
      // Direct transcoding here would be nice, however the .end() method that
      // we rely on is extremely slow in slice mode. serde_json only supports
      // iteration if we allow it to deserialize into an actual value, so jyt
      // implements a value type that can borrow strings from the input slice
      // (one of serde's major features).
      let de = serde_json::Deserializer::from_slice(&buf);
      for value in de.into_iter::<transcode::Value>() {
        output.transcode_value(value?)?;
      }
    }
    Input::Reader(r) => {
      // In this case, direct transcoding performs better than deserializing
      // into a value. Perhaps it can give the serializer &str slices of
      // internal buffers rather than allocating new Strings to store in a
      // transcode::Value?
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

impl<W: Write> crate::Output for Output<W> {
  fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<(), Box<dyn Error>>
  where
    D: serde::de::Deserializer<'de, Error = E>,
    E: serde::de::Error + 'static,
  {
    let mut ser = serde_json::Serializer::new(&mut self.0);
    transcode::transcode(de, &mut ser)?;
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
