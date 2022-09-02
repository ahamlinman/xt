//! The YAML data format.

use std::borrow::Cow;
use std::char::DecodeUtf16Error;
use std::error;
use std::fmt;
use std::io::{self, Write};
use std::str;

use serde::Deserialize;

use crate::{input, transcode};

pub(crate) fn input_matches(mut input: input::Ref) -> io::Result<bool> {
	// TODO: Is it worthwhile to avoid throwing away the result of a conversion?
	let input_str = match ensure_utf8(input.slice()?) {
		Ok(input_str) => input_str,
		Err(_) => return Ok(false),
	};
	let input_str = input_str.strip_prefix('\u{FEFF}').unwrap_or(&input_str);

	if let Some(de) = serde_yaml::Deserializer::from_str(input_str).next() {
		return Ok(serde::de::IgnoredAny::deserialize(de).is_ok());
	}
	Ok(false)
}

pub(crate) fn transcode<O>(input: input::Handle, mut output: O) -> crate::Result
where
	O: crate::Output,
{
	// serde-yaml imposes a couple of interesting limitations on us, which
	// aren't clear from the documentation alone but which are reflected in this
	// usage.
	//
	// First, while serde-yaml supports creating a Deserializer from a reader,
	// this actually just slurps the entire input into a byte vector and parses
	// the resulting slice. We would have to detect the splits between YAML
	// documents ourselves to do streaming input, either with some kind of text
	// stream processing (the evil way) or by implementing this as a real
	// feature upstream (the righteous way).
	//
	// Second, serde-yaml does not support UTF-16 or UTF-32 input, even though
	// YAML 1.2 requires this. While serde-yaml supports creating a Deserializer
	// from a &[u8], non-UTF-8 input will produce errors about control
	// characters or invalid UTF-8 octets. YAML has very clear rules for
	// encoding detection, so we re-encode the input ourselves if necessary.
	let input: Cow<'_, [u8]> = input.try_into()?;
	let input = ensure_utf8(&input)?;

	// YAML 1.2 allows for a BOM at the start of the stream, as well as at the
	// beginning of every subsequent document in a stream (though all documents
	// must use the same encoding). Unfortunately, serde-yaml seems to treat
	// BOMs like regular flow scalars (or something along those lines), and
	// documents with BOMs produce errors about mapping values not being
	// allowed. We take care of a single BOM at the start of a document since
	// it's pretty easy to handle, and hopefully covers most things (the
	// repeated BOM case seems to be about concatenating arbitrary documents, so
	// xt's multi-file support might be a useful workaround).
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
	fn transcode_from<'de, D, E>(&mut self, de: D) -> crate::Result
	where
		D: serde::de::Deserializer<'de, Error = E>,
		E: serde::de::Error + 'static,
	{
		writeln!(&mut self.0, "---")?;
		let mut ser = serde_yaml::Serializer::new(&mut self.0);
		transcode::transcode(&mut ser, de)?;
		Ok(())
	}

	fn transcode_value<S>(&mut self, value: S) -> crate::Result
	where
		S: serde::ser::Serialize,
	{
		writeln!(&mut self.0, "---")?;
		serde_yaml::to_writer(&mut self.0, &value)?;
		Ok(())
	}

	fn flush(&mut self) -> io::Result<()> {
		self.0.flush()
	}
}

/// Ensures that YAML input is UTF-8 by validating or converting it.
///
/// This function detects UTF-16 and UTF-32 input based on [section 5.2 of the
/// YAML v1.2.2 specification][spec]; detection behavior for non-YAML inputs is
/// not well defined. Conversions are optimized for simplicity and size, rather
/// than performance. Input that is invalid in the detected encoding, or whose
/// size does not evenly divide the detected code unit size, will return an
/// error.
///
/// [spec]: https://yaml.org/spec/1.2.2/#52-character-encodings
fn ensure_utf8(buf: &[u8]) -> Result<Cow<'_, str>, crate::Error> {
	let prefix = {
		// We use -1 as a sentinel for truncated input so the match patterns are
		// shorter to write than with Option<u8> variants.
		let mut result: [i16; 4] = Default::default();
		let mut iter = buf.iter();
		result.fill_with(|| iter.next().map(|x| i16::from(*x)).unwrap_or(-1));
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
		// The spec shows how to match a UTF-8 BOM, but since it's the same as
		// the default case there's no real point to an explicit check.
		_ => Cow::Borrowed(str::from_utf8(buf)?),
	})
}

fn convert_utf32<F>(buf: &[u8], get_u32: F) -> Result<String, EncodingError>
where
	F: Fn([u8; 4]) -> u32,
{
	if buf.len() % 4 != 0 {
		return Err(EncodingError::TruncatedUtf32);
	}

	// Start with just enough capacity for a pure ASCII result.
	let mut result = String::with_capacity(buf.len() / 4);
	for unit in buf.chunks_exact(4).map(|x| get_u32(x.try_into().unwrap())) {
		result.push(match char::from_u32(unit) {
			Some(chr) => chr,
			None => return Err(EncodingError::InvalidUtf32(unit)),
		});
	}
	Ok(result)
}

fn convert_utf16<F>(buf: &[u8], get_u16: F) -> Result<String, EncodingError>
where
	F: Fn([u8; 2]) -> u16,
{
	if buf.len() % 2 != 0 {
		return Err(EncodingError::TruncatedUtf16);
	}

	// Start with just enough capacity for a pure ASCII result.
	let mut result = String::with_capacity(buf.len() / 2);
	let units = buf.chunks_exact(2).map(|x| get_u16(x.try_into().unwrap()));
	for chr in char::decode_utf16(units) {
		result.push(match chr {
			Ok(chr) => chr,
			Err(err) => return Err(EncodingError::InvalidUtf16(err)),
		});
	}
	Ok(result)
}

/// An error encountered while decoding a UTF-16 or UTF-32 YAML document.
#[derive(Debug)]
enum EncodingError {
	TruncatedUtf16,
	TruncatedUtf32,
	InvalidUtf16(DecodeUtf16Error),
	InvalidUtf32(u32),
}

impl error::Error for EncodingError {}

impl fmt::Display for EncodingError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			EncodingError::TruncatedUtf16 => f.write_str("truncated utf-16"),
			EncodingError::TruncatedUtf32 => f.write_str("truncated utf-32"),
			EncodingError::InvalidUtf16(err) => write!(f, "invalid utf-16: {}", err),
			EncodingError::InvalidUtf32(unit) => write!(f, "invalid utf-32: {:x}", unit),
		}
	}
}
