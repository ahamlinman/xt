//! The YAML data format.

use std::io::{self, BufRead, BufReader, Write};
use std::str;

use serde::{de, ser};

use crate::input::{self, Input, Ref};
use crate::transcode;

mod chunker;
mod encoding;
#[allow(dead_code, clippy::undocumented_unsafe_blocks, clippy::todo)]
mod parser;

use self::chunker::Chunker;
use self::encoding::{Encoder, Encoding};

pub(crate) fn input_matches(mut input: Ref) -> io::Result<bool> {
	// YAML can be surprisingly liberal in what it accepts. In particular, the
	// contents of many non-YAML text documents can actually be parsed as YAML
	// scalars, such as TOML documents that do not start with a table. To
	// prevent these kinds of weird matches, we only detect input as YAML when
	// the first document in the stream encodes a collection (map or sequence).
	let encoding = Encoding::detect(input.prefix(Encoding::DETECT_LEN)?);
	let chunk = match &mut input {
		Ref::Slice(b) => Chunker::new(Encoder::new(b, encoding)).next(),
		Ref::Reader(r) => Chunker::new(Encoder::new(BufReader::new(r), encoding)).next(),
	};
	match chunk {
		Some(Ok(doc)) => Ok(!doc.is_scalar()),
		Some(Err(err)) if err.kind() == io::ErrorKind::InvalidData => Ok(false),
		Some(Err(err)) => Err(err),
		None => Ok(false),
	}
}

pub(crate) fn transcode<O>(input: input::Handle, mut output: O) -> crate::Result<()>
where
	O: crate::Output,
{
	match input.into() {
		Input::Reader(r) => transcode_reader(BufReader::new(r), output),
		Input::Slice(b) => match str::from_utf8(&b) {
			Ok(s) => {
				for de in serde_yaml::Deserializer::from_str(s) {
					output.transcode_from(de)?;
				}
				Ok(())
			}
			Err(_) => {
				// The reader path supports automatic re-encoding of UTF-16 and
				// UTF-32 input. See transcode_reader for details.
				transcode_reader(&*b, output)
			}
		},
	}
}

fn transcode_reader<R, O>(input: R, mut output: O) -> crate::Result<()>
where
	R: BufRead,
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
	for doc in Chunker::new(Encoder::from_reader(input)?) {
		let doc = doc?;
		let de = serde_yaml::Deserializer::from_str(doc.content());
		output.transcode_from(de)?;
	}
	Ok(())
}

pub(crate) struct Output<W: Write>(W);

impl<W: Write> Output<W> {
	pub(crate) fn new(w: W) -> Output<W> {
		Output(w)
	}
}

impl<W: Write> crate::Output for Output<W> {
	fn transcode_from<'de, D, E>(&mut self, de: D) -> crate::Result<()>
	where
		D: de::Deserializer<'de, Error = E>,
		E: de::Error + Send + Sync + 'static,
	{
		writeln!(&mut self.0, "---")?;
		let mut ser = serde_yaml::Serializer::new(&mut self.0);
		transcode::transcode(&mut ser, de)?;
		Ok(())
	}

	fn transcode_value<S>(&mut self, value: S) -> crate::Result<()>
	where
		S: ser::Serialize,
	{
		writeln!(&mut self.0, "---")?;
		serde_yaml::to_writer(&mut self.0, &value)?;
		Ok(())
	}

	fn flush(&mut self) -> io::Result<()> {
		self.0.flush()
	}
}
