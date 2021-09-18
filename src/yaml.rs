use std::error::Error;
use std::io::Write;

pub struct Output<W>(pub W);

impl<W> crate::Output for Output<W>
where
  W: Write,
{
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
