use std::borrow::Cow;
use std::convert::TryInto;
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
  // documents ourselves to do streaming input (unless, of course, we took on
  // the responsibility of implementing this upstream somehow).
  //
  // Second, yaml-rust does not support UTF-16 or UTF-32 input, even though YAML
  // 1.2 requires this. While serde_yaml supports creating a Deserializer from a
  // &[u8], this actually converts the slice to a &str internally. YAML has very
  // clear rules for encoding detection, so we re-encode the input ourselves if
  // necessary. We also strip any explicit byte order mark from the start of the
  // input, since while YAML 1.2 allows it (even for UTF-8 input) yaml-rust
  // doesn't seem to like it very much.
  let input = ensure_utf8(input.try_as_buffer()?)?;
  let input = strip_bom_if_present(&input);
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

/// Ensures that YAML input is UTF-8 by validating or converting it.
///
/// This function detects UTF-16 and UTF-32 input based on section 5.2 of the
/// YAML v1.2.2 specification; detection behavior for non-YAML inputs is not
/// well defined. Conversions are optimized for simplicity and size, rather than
/// performance. Input whose size does not evenly divide by the detected code
/// unit size will be truncated. Input that is invalid in the detected encoding
/// will return an error.
fn ensure_utf8<'a>(buf: &'a [u8]) -> Result<Cow<'a, str>, Box<dyn Error>> {
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
  // character, so that the pattern of null characters reveals the encoding.
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
  // UTF-8 requires at least one byte per code point, which gives us a pretty
  // obvious starting allocation for the happy path of a successful re-encoding.
  let mut result = String::with_capacity(buf.len() / 4);
  for unit in buf.chunks_exact(4).map(|v| get_u32(v.try_into().unwrap())) {
    let ch = char::from_u32(unit).ok_or_else(|| format!("invalid utf-32: {:x}", unit))?;
    result.push(ch);
  }
  Ok(result)
}

fn convert_utf16<F>(buf: &[u8], get_u16: F) -> Result<String, String>
where
  F: Fn([u8; 2]) -> u16,
{
  let mut result = String::with_capacity(buf.len() / 2);
  let units = buf.chunks_exact(2).map(|v| get_u16(v.try_into().unwrap()));
  for d in char::decode_utf16(units) {
    let ch = d.map_err(|err| format!("invalid utf-16: {}", err))?;
    result.push(ch);
  }
  Ok(result)
}

fn strip_bom_if_present(input: &str) -> &str {
  match input.strip_prefix("\u{FEFF}") {
    Some(stripped) => stripped,
    None => input,
  }
}
