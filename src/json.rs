use std::error::Error;
use std::io::{self, BufReader, Write};

use serde::Deserialize;

use crate::{transcode, BorrowedInput, Input, InputHandle};

pub(crate) fn input_matches(input: BorrowedInput) -> io::Result<bool> {
  let mut de = serde_json::Deserializer::from_reader(BufReader::new(input));
  if let Err(err) = serde::de::IgnoredAny::deserialize(&mut de) {
    return if err.is_io() {
      Err(err.into())
    } else {
      Ok(false)
    };
  }
  Ok(true)
}

pub(crate) fn transcode<O>(input: InputHandle, mut output: O) -> Result<(), Box<dyn Error>>
where
  O: crate::Output,
{
  match input.into() {
    Input::Buffer(buf) => {
      // Direct transcoding here would be nice, however the .end() method that
      // we rely on is extremely slow in slice mode. serde_json only supports
      // iteration if we allow it to deserialize into an actual value, so xt
      // implements a value type that can borrow strings from the input slice
      // (one of serde's major features).
      let de = serde_json::Deserializer::from_slice(&buf);
      for value in de.into_iter::<transcode::Value>() {
        output.transcode_value(value?)?;
      }
    }
    Input::Reader(r) => {
      // In this case, direct transcoding performs better than deserializing
      // into a value. It looks like transcode::Value is forced to copy every
      // string from a &str reference, which probably explains the difference.
      let mut de = serde_json::Deserializer::from_reader(BufReader::new(r));
      while de.end().is_err() {
        output.transcode_from(&mut de)?;
      }
    }
  }
  Ok(())
}

pub(crate) struct Output<W: Write>(W);

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
    transcode::transcode(&mut ser, de)?;
    writeln!(&mut self.0)?;
    Ok(())
  }

  fn transcode_value<S>(&mut self, value: S) -> Result<(), Box<dyn Error>>
  where
    S: serde::ser::Serialize,
  {
    serde_json::to_writer(&mut self.0, &value)?;
    writeln!(&mut self.0)?;
    Ok(())
  }
}
