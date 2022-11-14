#![deny(
	// Enforce some additional strictness on unsafe code.
	unsafe_op_in_unsafe_fn,
	clippy::undocumented_unsafe_blocks,
	// Deny a number of `as` casts in favor of safer alternatives.
	clippy::as_underscore,
	clippy::ptr_as_ptr,
	clippy::cast_lossless,
	clippy::cast_possible_truncation,
	clippy::checked_conversions,
	clippy::unnecessary_cast,
	// More general style-type things.
	clippy::from_over_into,
	clippy::semicolon_if_nothing_returned,
)]
#![warn(
	// Print macros can panic, and should only be for temporary debugging.
	clippy::print_stderr,
	clippy::print_stdout,
	// The following macros represent incomplete implementation work.
	clippy::todo,
	clippy::unimplemented,
)]

//! The main entrypoint for the xt serialized data translation tool.
//!
//! **This interface is not stable!** To convert between serialized data formats
//! in Rust code, consider [`serde_transcode`](https://docs.rs/serde-transcode)
//! as a more flexible and widely used implementation.

use std::fmt;
use std::io::{self, Read, Write};

use serde::{de, ser};

mod detect;
mod error;
mod input;
mod json;
mod msgpack;
mod toml;
mod transcode;
mod yaml;

pub use error::{Error, Result};

/// Translates the contents of a single input slice to a different format.
///
/// See [`Translator::translate_slice`].
pub fn translate_slice<W>(input: &[u8], from: Option<Format>, to: Format, output: W) -> Result<()>
where
	W: Write,
{
	Translator::new(output, to).translate_slice(input, from)
}

/// Translates the contents of a single reader to a different format.
///
/// See [`Translator::translate_reader`].
pub fn translate_reader<R, W>(input: R, from: Option<Format>, to: Format, output: W) -> Result<()>
where
	R: Read,
	W: Write,
{
	Translator::new(output, to).translate_reader(input, from)
}

/// Translates multiple inputs to a single serialized output.
///
/// A `Translator` accepts both slice and reader input. See [`translate_slice`]
/// and [`translate_reader`] for considerations associated with each kind of
/// source.
///
/// When a `Translator` is used more than once to translate different inputs, it
/// outputs the logical concatenation of all documents from all inputs as if
/// they had been presented in a single input. When translating to a format
/// without multi-document support, translation will fail if the translator
/// encounters more than one document in the first input, or if the translator
/// is called a second time with another input.
pub struct Translator<W>(Dispatcher<W>)
where
	W: Write;

impl<W> Translator<W>
where
	W: Write,
{
	/// Creates a translator that produces output in the given format.
	pub fn new(output: W, to: Format) -> Translator<W> {
		Translator(Dispatcher::new(output, to))
	}

	/// Translates the contents of a single input slice to a different format.
	///
	/// Slice inputs are typically more efficient to translate than reader
	/// inputs, but require all input to be available in memory in advance. For
	/// unbounded streams like standard input or non-regular files, consider
	/// using [`translate_reader`] rather than reading the entire stream into
	/// memory manually.
	///
	/// When `from` is `None`, xt will attempt to detect the input format from
	/// the input itself.
	pub fn translate_slice(&mut self, input: &[u8], from: Option<Format>) -> Result<()> {
		self.translate(input::Handle::from_slice(input), from)
	}

	/// Translates the contents of a single reader to a different format.
	///
	/// Reader inputs enable streaming translation for most formats, allowing xt
	/// to translate documents as they appear in the stream without buffering
	/// more than one document in memory at a time. When translating from a
	/// format that does not support streaming, xt will automatically read the
	/// entire input into memory before starting translation.
	///
	/// When `from` is `None`, xt will attempt to detect the input format from
	/// the input itself. The current format detection implementation does this
	/// by fully reading the contents of a single document into memory before
	/// starting translation.
	pub fn translate_reader<R>(&mut self, input: R, from: Option<Format>) -> Result<()>
	where
		R: Read,
	{
		self.translate(input::Handle::from_reader(input), from)
	}

	/// Translates a single serialized input to a different format.
	fn translate(&mut self, mut input: input::Handle<'_>, from: Option<Format>) -> Result<()> {
		let from = match from {
			Some(format) => format,
			None => match detect::detect_format(&mut input)? {
				Some(format) => format,
				None => return Err("unable to detect input format".into()),
			},
		};
		match from {
			Format::Json => json::transcode(input, &mut self.0),
			Format::Yaml => yaml::transcode(input, &mut self.0),
			Format::Toml => toml::transcode(input, &mut self.0),
			Format::Msgpack => msgpack::transcode(input, &mut self.0),
		}
	}

	/// [Flushes](Write::flush) the underlying writer.
	pub fn flush(&mut self) -> io::Result<()> {
		(&mut self.0).flush()
	}
}

/// A trait for output formats to receive their translatable input.
trait Output {
	fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<()>
	where
		D: de::Deserializer<'de, Error = E>,
		E: de::Error + Send + Sync + 'static;

	fn transcode_value<S>(&mut self, value: S) -> Result<()>
	where
		S: ser::Serialize;

	fn flush(&mut self) -> io::Result<()>;
}

/// An [`Output`] supporting static dispatch based on a known output format.
enum Dispatcher<W>
where
	W: Write,
{
	Json(json::Output<W>),
	Yaml(yaml::Output<W>),
	Toml(toml::Output<W>),
	Msgpack(msgpack::Output<W>),
}

impl<W> Dispatcher<W>
where
	W: Write,
{
	fn new(writer: W, to: Format) -> Dispatcher<W> {
		match to {
			Format::Json => Dispatcher::Json(json::Output::new(writer)),
			Format::Yaml => Dispatcher::Yaml(yaml::Output::new(writer)),
			Format::Toml => Dispatcher::Toml(toml::Output::new(writer)),
			Format::Msgpack => Dispatcher::Msgpack(msgpack::Output::new(writer)),
		}
	}
}

impl<W> Output for &mut Dispatcher<W>
where
	W: Write,
{
	fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<()>
	where
		D: de::Deserializer<'de, Error = E>,
		E: de::Error + Send + Sync + 'static,
	{
		match self {
			Dispatcher::Json(output) => output.transcode_from(de),
			Dispatcher::Yaml(output) => output.transcode_from(de),
			Dispatcher::Toml(output) => output.transcode_from(de),
			Dispatcher::Msgpack(output) => output.transcode_from(de),
		}
	}

	fn transcode_value<S>(&mut self, value: S) -> Result<()>
	where
		S: ser::Serialize,
	{
		match self {
			Dispatcher::Json(output) => output.transcode_value(value),
			Dispatcher::Yaml(output) => output.transcode_value(value),
			Dispatcher::Toml(output) => output.transcode_value(value),
			Dispatcher::Msgpack(output) => output.transcode_value(value),
		}
	}

	fn flush(&mut self) -> io::Result<()> {
		match self {
			Dispatcher::Json(output) => output.flush(),
			Dispatcher::Yaml(output) => output.flush(),
			Dispatcher::Toml(output) => output.flush(),
			Dispatcher::Msgpack(output) => output.flush(),
		}
	}
}

/// The set of input and output formats supported by xt.
///
/// Support for each format comes largely from external crates, with some
/// additional preprocessing by xt for select formats. The crate selection for
/// each format is **not stable**, and is documented for informational purposes
/// only.
#[derive(Copy, Clone)]
#[non_exhaustive]
pub enum Format {
	/// The [JSON][json] format as interpreted by [`serde_json`].
	///
	/// This format supports multi-document translation and streaming input.
	///
	/// [json]: https://datatracker.ietf.org/doc/html/rfc8259
	Json,
	/// The [YAML 1.2][yaml] format as interpreted by [`serde_yaml`].
	///
	/// This format supports multi-document translation and streaming input.
	///
	/// [yaml]: https://yaml.org/spec/1.2.2/
	Yaml,
	/// The [TOML][toml] format as interpreted by [`toml`][::toml].
	///
	/// This format supports single-document translation only, and as such does
	/// not support streaming input.
	///
	/// [toml]: https://github.com/toml-lang/toml
	Toml,
	/// The [MessagePack][msgpack] format as interpreted by [`rmp_serde`].
	///
	/// This format supports multi-document translation and streaming input.
	///
	/// [msgpack]: https://msgpack.org/
	Msgpack,
}

impl fmt::Display for Format {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		f.write_str(match self {
			Self::Json => "JSON",
			Self::Yaml => "YAML",
			Self::Toml => "TOML",
			Self::Msgpack => "MessagePack",
		})
	}
}
