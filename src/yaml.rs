//! The YAML data format.

use std::borrow::Cow;
use std::io::{self, Read, Write};
use std::str;

use serde::Deserialize;

use crate::{input, transcode};

mod encoding;

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
