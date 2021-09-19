use std::error::Error;
use std::io::Write;

use crate::TranscodeFrom;

pub(crate) fn transcode<T>(input: &[u8], mut output: T) -> Result<(), Box<dyn Error>>
where
  T: TranscodeFrom,
{
  let mut de = serde_json::Deserializer::from_slice(input);
  while let Err(_) = de.end() {
    output.transcode_from(&mut de)?;
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
}
