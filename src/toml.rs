//! The TOML data format.

use std::borrow::Cow;
use std::error;
use std::fmt;
use std::io::{self, Write};
use std::str;

use serde::{de, ser, Deserialize};

use crate::input::{self, Ref};

pub(crate) fn input_matches(mut input: Ref) -> io::Result<bool> {
	let input_buf = match input {
		Ref::Slice(b) => b,
		Ref::Reader(_) => {
			// Our TOML parser requires that we fully buffer all input into a
			// &str before parsing. However, if we have an unbounded input
			// stream, we don't just want to endlessly fill up some poor buffer
			// until we finally crash from an allocation failure.
			//
			// As an arbitrary cutoff, let's say that if you're streaming a TOML
			// document >= 2 MiB in size into xt, it might be time to stop and
			// think about some things.
			const SIZE_CUTOFF: usize = 2 * 1024_usize.pow(2);
			let prefix = input.prefix(SIZE_CUTOFF)?;
			if prefix.len() >= SIZE_CUTOFF {
				return Ok(false);
			}
			prefix
		}
	};

	let input_str = match str::from_utf8(input_buf) {
		Ok(input_str) => input_str,
		Err(_) => return Ok(false),
	};

	let de = ::toml::Deserializer::new(input_str);
	Ok(de::IgnoredAny::deserialize(de).is_ok())
}

pub(crate) fn transcode<O>(input: input::Handle, mut output: O) -> crate::Result<()>
where
	O: crate::Output,
{
	let input: Cow<'_, [u8]> = input.try_into()?;
	let de = ::toml::Deserializer::new(str::from_utf8(&input)?);
	output.transcode_from(de)
}

pub(crate) struct Output<W: Write> {
	w: W,
	used: bool,
}

impl<W: Write> Output<W> {
	pub(crate) fn new(w: W) -> Output<W> {
		Output { w, used: false }
	}

	fn ensure_one_use(&mut self) -> crate::Result<()> {
		// Since TOML has no concept of multiple documents in a single stream,
		// and we can't know the number of input documents in advance, we just
		// fail if someone tries to use us more than once. We try to run this
		// check before we even deserialize any values, so we don't waste time
		// on things that will get thrown out.
		if self.used {
			return Err(TomlOutputError::MultiDocument.into());
		}
		self.used = true;
		Ok(())
	}

	fn output_value(&mut self, value: &::toml::Value) -> crate::Result<()> {
		// TOML requires that the root of the document be a table, which we
		// can't guarantee for arbitrary input. While the toml crate allows
		// deserializing to a toml::Table to avoid accepting non-table root
		// values, the error message for such values (as of this writing) can be
		// a bit noisier than the one we've historically provided ourselves, so
		// we instead deserialize into the more general toml::Value and run the
		// check manually.
		//
		// TOML also requires that non-table values appear before any tables at
		// a given level of nesting, which the toml crate knows how to handle.
		// We enable the crate's "preserve_order" feature to keep as much of the
		// original input ordering as we can.
		if let toml::Value::Table(table) = value {
			write!(&mut self.w, "{table}").map_err(Into::into)
		} else {
			Err(TomlOutputError::NonTableRoot.into())
		}
	}
}

impl<W: Write> crate::Output for Output<W> {
	fn transcode_from<'de, D, E>(&mut self, de: D) -> crate::Result<()>
	where
		D: de::Deserializer<'de, Error = E>,
		E: de::Error + Send + Sync + 'static,
	{
		self.ensure_one_use()?;
		let value = ::toml::Value::deserialize(de)?;
		self.output_value(&value)
	}

	fn transcode_value<S>(&mut self, value: S) -> crate::Result<()>
	where
		S: ser::Serialize,
	{
		self.ensure_one_use()?;
		let value = ::toml::Value::try_from(value)?;
		self.output_value(&value)
	}

	fn flush(&mut self) -> io::Result<()> {
		self.w.flush()
	}
}

/// An error encountered while translating a document to TOML.
#[derive(Debug)]
enum TomlOutputError {
	NonTableRoot,
	MultiDocument,
}

impl error::Error for TomlOutputError {}

impl fmt::Display for TomlOutputError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			TomlOutputError::NonTableRoot => f.write_str("root of TOML output must be a table"),
			TomlOutputError::MultiDocument => {
				f.write_str("TOML does not support multi-document output")
			}
		}
	}
}
