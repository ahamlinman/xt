use std::error::Error;
use std::io::Write;

use crate::{transcode, InputHandle};

pub(crate) fn transcode<O>(mut input: InputHandle, mut output: O) -> Result<(), Box<dyn Error>>
where
  O: crate::Output,
{
  // serde_yaml imposes a couple of interesting limitations on us, which aren't
  // clear from the documentation alone but which are reflected in jyt's usage.
  //
  // First, while serde_yaml supports creating a Deserializer from a reader,
  // this actually just slurps the entire input into a byte vector and parses
  // the resulting slice. We would have to detect the splits between YAML
  // documents ourselves to do streaming input.
  //
  // Second, yaml-rust does not actually implement the full YAML 1.2 spec, as it
  // does not support character encodings other than UTF-8. serde_yaml will
  // always try to convert a byte slice to a &str before parsing it. We would
  // have to implement YAML's encoding detection rules and re-encode each
  // document ourselves to support other encodings.
  //
  // Of course, the suggestion that jyt work around these limitations internally
  // does not preclude being a good citizen and contributing such improvements
  // upstream in the future. On the contrary, a proven real-world implementation
  // within jyt could be a great starting point.
  for de in serde_yaml::Deserializer::from_slice(input.try_as_buffer()?) {
    output.transcode_from(de)?;
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
    let mut ser = serde_yaml::Serializer::new(&mut self.0);
    transcode::transcode(de, &mut ser)?;
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
