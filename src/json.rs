//! The JSON data format.

use std::error::Error;
use std::io::{self, BufReader, Read, Write};

use serde::Deserialize;

use crate::input::{self, Input};
use crate::transcode;

pub(crate) fn input_matches(mut input: input::Ref) -> io::Result<bool> {
	let result = match &mut input {
		input::Ref::Slice(b) => match_input_buffer(b),
		input::Ref::Reader(r) => match_input_reader(r),
	};
	match result {
		Err(err) if err.is_io() => Err(err.into()),
		Err(_) => Ok(false),
		Ok(()) => Ok(true),
	}
}

fn match_input_buffer(input: &[u8]) -> Result<(), serde_json::Error> {
	let mut de = serde_json::Deserializer::from_slice(input);
	serde::de::IgnoredAny::deserialize(&mut de).and(Ok(()))
}

fn match_input_reader<R: Read>(input: R) -> Result<(), serde_json::Error> {
	let mut de = serde_json::Deserializer::from_reader(input);
	serde::de::IgnoredAny::deserialize(&mut de).and(Ok(()))
}

pub(crate) fn transcode<O>(input: input::Handle, mut output: O) -> Result<(), Box<dyn Error>>
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
			let de = serde_json::Deserializer::from_slice(&b);
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
		let mut ser = serde_json::Serializer::new(&mut self.0);
		transcode::transcode(&mut ser, de)?;
		writeln!(&mut self.0)?;
		Ok(())
	}

	fn transcode_value<S>(&mut self, value: S) -> Result<(), Box<dyn Error>>
	where
		S: serde::ser::Serialize,
	{
		serde_json::to_writer(&mut self.0, &value)?;
		writeln!(&mut self.0)?;
		Ok(())
	}
}
