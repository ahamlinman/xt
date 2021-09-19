use std::error::Error;
use std::io::Write;

use crate::InputRef;

pub(crate) fn transcode<O>(mut input: InputRef, mut output: O) -> Result<(), Box<dyn Error>>
where
  O: crate::Output,
{
  for de in serde_yaml::Deserializer::from_slice(input.try_buffer()?) {
    output.transcode_from(de)?;
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
    let mut ser = serde_yaml::Serializer::new(&mut self.0);
    serde_transcode::transcode(de, &mut ser)?;
    Ok(())
  }

  fn transcode_value<S>(&mut self, value: S) -> Result<(), Box<dyn Error>>
  where
    S: serde::ser::Serialize,
  {
    serde_yaml::to_writer(&mut self.0, &value)?;
    Ok(())
  }
}
