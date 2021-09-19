use std::error::Error;
use std::io::Write;

use crate::{Input, TranscodeFrom};

pub(crate) fn transcode<T>(mut input: Input, mut output: T) -> Result<(), Box<dyn Error>>
where
  T: TranscodeFrom,
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

impl<W: Write> TranscodeFrom for Output<W> {
  fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<(), Box<dyn Error>>
  where
    D: serde::de::Deserializer<'de, Error = E>,
    E: serde::de::Error + 'static,
  {
    let mut ser = serde_yaml::Serializer::new(&mut self.0);
    serde_transcode::transcode(de, &mut ser)?;
    Ok(())
  }
}
