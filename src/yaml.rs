use std::borrow::Cow;
use std::convert::TryInto;
use std::error::Error;
use std::io::Write;
use std::string::FromUtf16Error;

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
  // documents ourselves to do streaming input (unless, of course, we took on
  // the responsibility of implementing this upstream somehow).
  //
  // Second, yaml-rust does not support UTF-16 or UTF-32 input, even though YAML
  // 1.2 requires this. While serde_yaml supports creating a Deserializer from a
  // &[u8], this actually converts the slice to a &str internally. YAML has very
  // clear rules for encoding detection, so we re-encode the input ourselves if
  // necessary.
  let input = ensure_utf8(input.try_as_buffer()?)?;
  for de in serde_yaml::Deserializer::from_str(&input) {
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

/// Ensures that YAML input is UTF-8 by validating or re-encoding it.
///
/// This function detects UTF-16 and UTF-32 input based on section 5.2 of the
/// YAML v1.2.2 specification. The conversions are optimized for simplicity and
/// size, rather than performance or exhaustive correctness. Input whose size
/// does not evenly divide by the detected code unit size will be truncated.
/// Input that is invalid in the detected encoding will return an error.
fn ensure_utf8<'a>(buf: &'a [u8]) -> Result<Cow<'a, str>, Box<dyn Error>> {
  let prefix = {
    // We use -1 as the sentinel so it makes the match conditions shorter to
    // write, vs. if we did Option<u8> and had to match Some variants for every
    // byte.
    let mut result: [i16; 4] = Default::default();
    let mut iter = buf.iter().take(4);
    result.fill_with(|| iter.next().copied().map(|x| x as i16).unwrap_or(-1));
    result
  };

  // See https://yaml.org/spec/1.2.2/#52-character-encodings.
  Ok(match prefix {
    [0, 0, 0xFE, 0xFF] | [0, 0, 0, _] => Cow::Owned(convert_utf32(buf, u32::from_be_bytes)?),
    [0xFF, 0xFE, 0, 0] | [_, 0, 0, 0] => Cow::Owned(convert_utf32(buf, u32::from_le_bytes)?),
    [0xFE, 0xFF, _, _] | [0, _, _, _] => Cow::Owned(convert_utf16(buf, u16::from_be_bytes)?),
    [0xFF, 0xFE, _, _] | [_, 0, _, _] => Cow::Owned(convert_utf16(buf, u16::from_le_bytes)?),
    _ => Cow::Borrowed(std::str::from_utf8(buf)?),
  })
}

fn convert_utf32<F>(buf: &[u8], get_u32: F) -> Result<String, String>
where
  F: Fn([u8; 4]) -> u32,
{
  // We'll start the string out with the minimum possible capacity for a
  // successful UTF-8 re-encoding.
  let mut result = String::with_capacity(buf.len() / 4);
  for (i, chunk) in buf.chunks_exact(4).enumerate() {
    let v = get_u32(chunk.try_into().unwrap());
    let c = match char::from_u32(v) {
      Some(c) => c,
      None => return Err(format!("invalid utf-32: 0x{:04x} at byte {}", v, i * 4)),
    };
    result.push(c);
  }
  Ok(result)
}

fn convert_utf16<F>(buf: &[u8], get_u16: F) -> Result<String, FromUtf16Error>
where
  F: Fn([u8; 2]) -> u16,
{
  let units: Vec<u16> = buf
    .chunks_exact(2)
    .into_iter()
    .map(|chunk| get_u16(chunk.try_into().unwrap()))
    .collect();
  String::from_utf16(&units)
}
