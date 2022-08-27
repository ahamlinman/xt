//! The TOML data format.

use std::borrow::Cow;
use std::error;
use std::fmt;
use std::io::{self, Write};
use std::str;

use serde::Deserialize;

use crate::input;

pub(crate) fn input_matches(mut input: input::Ref) -> io::Result<bool> {
	// TODO: Can we avoid throwing away the result of the UTF-8 check?
	let input_str = match str::from_utf8(input.slice()?) {
		Ok(input_str) => input_str,
		Err(_) => return Ok(false),
	};
	let mut de = ::toml::Deserializer::new(input_str);
	Ok(serde::de::IgnoredAny::deserialize(&mut de).is_ok())
}

pub(crate) fn transcode<O>(input: input::Handle, mut output: O) -> Result<(), crate::Error>
where
	O: crate::Output,
{
	let input: Cow<'_, [u8]> = input.try_into()?;
	let input_str = str::from_utf8(&input)?; // TOML is UTF-8 only, always.
	let mut de = ::toml::Deserializer::new(input_str);
	output.transcode_from(&mut de)
}

pub(crate) struct Output<W: Write> {
	w: W,
	used: bool,
}

impl<W: Write> Output<W> {
	pub fn new(w: W) -> Output<W> {
		Output { w, used: false }
	}
}

impl<W: Write> crate::Output for Output<W> {
	fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<(), crate::Error>
	where
		D: serde::de::Deserializer<'de, Error = E>,
		E: serde::de::Error + 'static,
	{
		// TOML is pretty unique among xt's supported output formats, and
		// requires some special considerations.

		// Since TOML has no concept of multiple documents in a single stream,
		// and we can't know the number of input documents in advance, we just
		// fail if someone tries to use us more than once. (We try to put this
		// check first so we don't waste time on a value that will get thrown
		// out.)
		self.ensure_one_use()?;

		// TOML requires that non-table values appear before any tables at a
		// given nesting level, which we cannot guarantee for arbitrary input.
		// toml::Value knows how to deal with this. We enable the toml crate's
		// "preserve_order" feature to keep as much of the original input
		// ordering as we can.
		let value = ::toml::Value::deserialize(de)?;

		// There are a couple more small details, see output_value for details.
		self.output_value(&value)
	}

	fn transcode_value<S>(&mut self, value: S) -> Result<(), crate::Error>
	where
		S: serde::ser::Serialize,
	{
		// Of course, all of the above comments apply here as well.
		self.ensure_one_use()?;
		let value = ::toml::Value::try_from(value)?;
		self.output_value(&value)
	}
}

impl<W: Write> Output<W> {
	fn output_value(&mut self, value: &::toml::Value) -> Result<(), crate::Error> {
		// From the spec: "TOML is designed to map unambiguously to a hash
		// table." xt's other input formats can produce something like a boolean
		// or array at the top level, which might make xt output an invalid TOML
		// document if we're not careful. For example, I have seen the toml
		// crate dump a top-level array as an array of tables with an empty
		// name, i.e. with "[[]]" headers.
		if !value.is_table() {
			return Err(NonTableRootError.into());
		}

		// As of this writing, the toml crate can't output directly to a writer.
		let output_buf = ::toml::to_string_pretty(&value)?;
		self.w.write_all(output_buf.as_bytes())?;
		Ok(())
	}

	fn ensure_one_use(&mut self) -> Result<(), &'static str> {
		if self.used {
			return Err("TOML does not support multi-document output");
		}
		self.used = true;
		Ok(())
	}
}

#[derive(Debug)]
struct NonTableRootError;

impl fmt::Display for NonTableRootError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		f.write_str("root of TOML output must be a table")
	}
}

impl error::Error for NonTableRootError {}
