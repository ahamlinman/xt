//! The JSON data format.

use std::io::{self, BufReader, Read, Write};
use std::str;

use serde::{de, ser, Deserialize};

use crate::input::{self, Input, Ref};
use crate::transcode;

pub(crate) fn input_matches(mut input: Ref) -> io::Result<bool> {
	let result = match &mut input {
		Ref::Reader(r) => match_input_reader(r),
		Ref::Slice(b) => match str::from_utf8(b) {
			Ok(s) => match_input_str(s),
			Err(_) => return Ok(false),
		},
	};
	match result {
		Err(err) if err.is_io() => Err(err.into()),
		Err(_) => Ok(false),
		Ok(()) => Ok(true),
	}
}

fn match_input_str(input: &str) -> Result<(), serde_json::Error> {
	let mut de = serde_json::Deserializer::from_str(input);
	de::IgnoredAny::deserialize(&mut de).and(Ok(()))
}

fn match_input_reader<R: Read>(input: R) -> Result<(), serde_json::Error> {
	let mut de = serde_json::Deserializer::from_reader(input);
	de::IgnoredAny::deserialize(&mut de).and(Ok(()))
}

pub(crate) fn transcode<O>(input: input::Handle, mut output: O) -> crate::Result<()>
where
	O: crate::Output,
{
	match input.into() {
		Input::Slice(b) => {
			// Direct transcoding here would be nice, however the .end() method
			// that we rely on is extremely slow in slice mode. serde_json only
			// supports iteration if we allow it to deserialize into an actual
			// value, so xt implements a value type that can borrow strings from
			// the input slice (one of serde's major features).
			//
			// Per RFC 8259: "JSON text exchanged between systems that are not
			// part of a closed ecosystem MUST be encoded using UTF-8." In my
			// testing, an upfront UTF-8 check on the entire input nets as much
			// as a 20% performance improvement compared to allowing serde_json
			// to check UTF-8 validity as it parses a byte slice. The drawback
			// is that UTF-8 encoding errors will only provide a byte position
			// rather than full line and column information. In this case, I
			// think the performance improvement is the more justifiable option.
			let de = serde_json::Deserializer::from_str(str::from_utf8(&b)?);
			for value in de.into_iter::<transcode::Value>() {
				output.transcode_value(value?)?;
			}
		}
		Input::Reader(r) => {
			// Direct transcoding here performs better than deserializing into a
			// value. It looks like transcode::Value is forced to copy every
			// string from a &str reference, which probably explains the
			// difference.
			let mut de = serde_json::Deserializer::from_reader(BufReader::new(r));
			while de.end().is_err() {
				output.transcode_from(&mut de)?;
			}
		}
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
		let mut ser = serde_json::Serializer::new(&mut self.0);
		transcode::transcode(&mut ser, de)?;
		writeln!(&mut self.0)?;
		Ok(())
	}

	fn transcode_value<S>(&mut self, value: S) -> crate::Result<()>
	where
		S: ser::Serialize,
	{
		serde_json::to_writer(&mut self.0, &value)?;
		writeln!(&mut self.0)?;
		Ok(())
	}

	fn flush(&mut self) -> io::Result<()> {
		self.0.flush()
	}
}
