use std::error::Error;
use std::io::{BufReader, Write};

use crate::{transcode, Input, InputRef};

pub(crate) fn transcode<O>(input: InputRef, mut output: O) -> Result<(), Box<dyn Error>>
where
  O: crate::Output,
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
      for value in de.into_iter::<transcode::Value>() {
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

impl<W: Write> crate::Output for Output<W> {
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
