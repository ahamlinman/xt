//! The YAML data format.

use std::borrow::Cow;
use std::io::{self, BufReader, Read, Write};
use std::str;

use serde::Deserialize;

use crate::input::{self, Input};
use crate::transcode;

mod chunker;
mod encoding;

use self::chunker::Chunker;
use self::encoding::{Encoder, Encoding};

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
	// serde_yaml imposes a couple of interesting limitations on us, which
	// aren't clear from the documentation alone but are reflected in this
	// usage.
	//
	// First, while serde_yaml supports creating a Deserializer from a reader,
	// this actually slurps the entire input into a buffer for parsing. We
	// support streaming parsing by implementing our own "chunker" that splits
	// an unbounded YAML stream into a sequence of buffered documents. This is a
	// terrible hack, and I sincerely hope that I will have the time and energy
	// someday to implement true streaming support in serde_yaml.
	//
	// Second, serde_yaml does not support UTF-16 or UTF-32 input, even though
	// YAML 1.2 requires this. In addition to our chunker, we implement a
	// streaming encoder that can detect the encoding of any valid YAML stream
	// and convert it to UTF-8. The encoder will also strip any byte order mark
	// from the beginning of the stream, as serde_yaml will choke on it. This
	// still doesn't cover the full YAML spec, which also allows BOMs in UTF-8
	// streams and at the starts of individual documents in the stream.
	// However, these cases should be much rarer than that of a single BOM at
	// the start of a UTF-16 or UTF-32 stream.

	match input.into() {
		Input::Slice(b) => {
			for de in serde_yaml::Deserializer::from_str(&ensure_utf8(&b)?) {
				output.transcode_from(de)?;
			}
		}
		Input::Reader(r) => {
			for chunk in Chunker::new(Encoder::from_reader(BufReader::new(r))?) {
				output.transcode_from(serde_yaml::Deserializer::from_str(&chunk?))?;
			}
		}
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
/// See [Encoding::detect] for details on the detection algorithm.
fn ensure_utf8(buf: &[u8]) -> Result<Cow<'_, str>, crate::Error> {
	match Encoding::detect(buf) {
		Encoding::Utf8 => Ok(Cow::Borrowed(str::from_utf8(buf)?)),
		encoding => {
			let mut result = String::with_capacity(match encoding {
				Encoding::Utf8 => unreachable!(),
				Encoding::Utf16Big | Encoding::Utf16Little => buf.len() / 2,
				Encoding::Utf32Big | Encoding::Utf32Little => buf.len() / 4,
			});
			Encoder::new(buf, encoding).read_to_string(&mut result)?;
			Ok(Cow::Owned(result))
		}
	}
}
