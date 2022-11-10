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

//! The main entrypoint for the xt serialized data translation tool.
//!
//! **This interface is not stable!** To convert between serialized data formats
//! in Rust code, consider [`serde_transcode`](https://docs.rs/serde-transcode)
//! as a more flexible and widely used implementation.

use std::error;
use std::fmt;
use std::io::{self, Write};
use std::result;

mod detect;
mod input;
mod json;
mod msgpack;
mod toml;
mod transcode;
mod yaml;

pub use input::Handle;

/// The result produced by a translation.
///
/// There is no useful `Ok` value, as the translator streams its output to a
/// writer.
pub type Result = result::Result<(), Error>;

/// An error produced during a translation.
pub type Error = Box<dyn error::Error>;

/// Translates a single serialized input to serialized output in a different
/// format.
///
/// When `from` is `None`, xt will attempt to detect the input format using an
/// unspecified and unstable algorithm.
pub fn translate<W>(input: Handle<'_>, from: Option<Format>, to: Format, output: W) -> crate::Result
where
	W: Write,
{
	Translator::new(output, to).translate(input, from)
}

/// Translates multiple inputs to a single serialized output.
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

	/// Translates serialized input to serialized output in the translator's
	/// output format.
	///
	/// When `from` is `None`, xt will attempt to detect the input format using an
	/// unspecified and unstable algorithm.
	pub fn translate(&mut self, mut input: Handle<'_>, from: Option<Format>) -> crate::Result {
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
	fn transcode_from<'de, D, E>(&mut self, de: D) -> crate::Result
	where
		D: serde::de::Deserializer<'de, Error = E>,
		E: serde::de::Error + 'static;

	fn transcode_value<S>(&mut self, value: S) -> crate::Result
	where
		S: serde::ser::Serialize;

	fn flush(&mut self) -> io::Result<()>;
}

/// An [`Output`] implementation supporting static dispatch based on a known
/// output format.
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
	fn transcode_from<'de, D, E>(&mut self, de: D) -> crate::Result
	where
		D: serde::de::Deserializer<'de, Error = E>,
		E: serde::de::Error + 'static,
	{
		match self {
			Dispatcher::Json(output) => output.transcode_from(de),
			Dispatcher::Yaml(output) => output.transcode_from(de),
			Dispatcher::Toml(output) => output.transcode_from(de),
			Dispatcher::Msgpack(output) => output.transcode_from(de),
		}
	}

	fn transcode_value<S>(&mut self, value: S) -> crate::Result
	where
		S: serde::ser::Serialize,
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
/// The feature sets of the supported formats vary along two key dimensions.
///
/// Formats supporting **multi-document translation** can represent multiple
/// independent documents in a single input. For example, xt can translate a
/// single YAML file with documents separated by `---` markers into a stream of
/// newline-delimited JSON values. In contrast, single document formats like
/// TOML can only represent one document per input. When translating multiple
/// documents to a single document output format (whether from multiple inputs
/// or a single multi-document input), xt will return an error as it starts to
/// translate the second document in the stream.
///
/// Formats supporting **streaming input** can translate a multi-document input
/// stream without first buffering the entire stream into memory. This enables
/// translation of unbounded input sources, such as a program providing input to
/// xt through a pipe. Formats that do not support streaming must buffer the
/// entire input stream into memory before translating the first document.
///
/// Support for each data format comes largely from external Rust crates, with
/// minor additional preprocessing from xt for select formats. Note that the
/// crate selection for each format is **not stable**, and is documented for
/// informational purposes only.
#[derive(Copy, Clone)]
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
	/// This format supports single-document translation only, and does not
	/// support streaming input.
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
