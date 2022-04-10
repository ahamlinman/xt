use std::borrow::Cow;
use std::error::Error;
use std::io::Write;

use crate::{transcode, InputHandle};

pub(crate) fn transcode<O>(mut input: InputHandle, mut output: O) -> Result<(), Box<dyn Error>>
where
  O: crate::Output,
{
  // serde_yaml imposes a couple of interesting limitations on us, which aren't
  // clear from the documentation alone but which are reflected in this usage.
  //
  // First, while serde_yaml supports creating a Deserializer from a reader,
  // this actually just slurps the entire input into a byte vector and parses
  // the resulting slice. We would have to detect the splits between YAML
  // documents ourselves to do streaming input, either with some kind of text
  // stream processing (the evil way) or by implementing this as a real feature
  // upstream (the righteous way).
  //
  // Second, yaml-rust does not support UTF-16 or UTF-32 input, even though YAML
  // 1.2 requires this. While serde_yaml supports creating a Deserializer from a
  // &[u8], this actually converts the slice to a &str internally. YAML has very
  // clear rules for encoding detection, so we re-encode the input ourselves if
  // necessary.
  let input = ensure_utf8(input.try_as_buffer()?)?;

  // YAML 1.2 allows for a BOM at the start of the stream, as well as at the
  // beginning of every subsequent document in a stream (though all documents
  // must use the same encoding). Unfortunately, yaml-rust seems to treat BOMs
  // like syntax errors regardless of where they show up. We take care of the
  // former case since it's pretty easy to handle, and hopefully covers most
  // things.
  let input = input.strip_prefix('\u{FEFF}').unwrap_or(&input);

  for de in serde_yaml::Deserializer::from_str(input) {
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
    transcode::transcode(&mut ser, de)?;
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

/// Ensures that YAML input is UTF-8 by validating or converting it.
///
/// This function detects UTF-16 and UTF-32 input based on section 5.2 of the
/// YAML v1.2.2 specification; detection behavior for non-YAML inputs is not
/// well defined. Conversions are optimized for simplicity and size, rather than
/// performance. Input that is invalid in the detected encoding, or whose size
/// does not evenly divide the detected code unit size, will return an error.
fn ensure_utf8(buf: &[u8]) -> Result<Cow<'_, str>, Box<dyn Error>> {
  let prefix = {
    // We use -1 as a sentinel for truncated input so the match patterns are
    // shorter to write than with Option<u8> variants.
    let mut result: [i16; 4] = Default::default();
    let mut iter = buf.iter();
    result.fill_with(|| iter.next().map(|x| *x as i16).unwrap_or(-1));
    result
  };

  // See https://yaml.org/spec/1.2.2/#52-character-encodings. Notably, valid
  // YAML streams that do not begin with a BOM must begin with an ASCII
  // character, so that the pattern of null bytes reveals the encoding.
  Ok(match prefix {
    [0, 0, 0xFE, 0xFF] | [0, 0, 0, _] if buf.len() >= 4 => {
      Cow::Owned(convert_utf32(buf, u32::from_be_bytes)?)
    }
    [0xFF, 0xFE, 0, 0] | [_, 0, 0, 0] if buf.len() >= 4 => {
      Cow::Owned(convert_utf32(buf, u32::from_le_bytes)?)
    }
    [0xFE, 0xFF, ..] | [0, _, ..] if buf.len() >= 2 => {
      Cow::Owned(convert_utf16(buf, u16::from_be_bytes)?)
    }
    [0xFF, 0xFE, ..] | [_, 0, ..] if buf.len() >= 2 => {
      Cow::Owned(convert_utf16(buf, u16::from_le_bytes)?)
    }
    // The spec shows how to match a UTF-8 BOM, but since it's the same as the
    // default case there's no real point to an explicit check.
    _ => Cow::Borrowed(std::str::from_utf8(buf)?),
  })
}

fn convert_utf32<F>(buf: &[u8], get_u32: F) -> Result<String, String>
where
  F: Fn([u8; 4]) -> u32,
{
  if buf.len() % 4 != 0 {
    return Err("truncated utf-32".into());
  }

  // Start with just enough capacity for a pure ASCII result.
  let mut result = String::with_capacity(buf.len() / 4);
  for unit in buf.chunks_exact(4).map(|x| get_u32(x.try_into().unwrap())) {
    result.push(match char::from_u32(unit) {
      Some(chr) => chr,
      None => return Err(format!("invalid utf-32: {:x}", unit)),
    })
  }
  Ok(result)
}

fn convert_utf16<F>(buf: &[u8], get_u16: F) -> Result<String, String>
where
  F: Fn([u8; 2]) -> u16,
{
  if buf.len() % 2 != 0 {
    return Err("truncated utf-16".into());
  }

  // Start with just enough capacity for a pure ASCII result.
  let mut result = String::with_capacity(buf.len() / 2);
  let units = buf.chunks_exact(2).map(|x| get_u16(x.try_into().unwrap()));
  for chr in char::decode_utf16(units) {
    result.push(match chr {
      Ok(chr) => chr,
      Err(err) => return Err(format!("invalid utf-16: {}", err)),
    })
  }
  Ok(result)
}
