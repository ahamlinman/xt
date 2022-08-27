#![deny(
	clippy::cast_lossless,
	clippy::cast_possible_truncation,
	clippy::enum_glob_use,
	clippy::from_over_into,
	clippy::semicolon_if_nothing_returned
)]

//! The main entrypoint for the xt serialized data translation tool.
//!
//! **This interface is not stable!** To convert between serialized data formats
//! in Rust code, consider [`serde_transcode`](https://docs.rs/serde-transcode)
//! as a more flexible and widely used implementation.

use std::error;
use std::fmt;
use std::io::Write;

mod detect;
mod input;
mod json;
mod msgpack;
mod toml;
mod transcode;
mod yaml;

pub use input::Handle;

/// Translates a single serialized input to serialized output in a different
/// format.
///
/// When `from` is `None`, xt will attempt to detect the input format using an
/// unspecified and unstable algorithm.
pub fn translate<W>(
	input: Handle<'_>,
	from: Option<Format>,
	to: Format,
	output: W,
) -> Result<(), Box<dyn error::Error>>
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
	pub fn translate(
		&mut self,
		mut input: Handle<'_>,
		from: Option<Format>,
	) -> Result<(), Box<dyn error::Error>> {
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
}

/// A trait for output formats to receive their translatable input.
trait Output {
	fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<(), Box<dyn error::Error>>
	where
		D: serde::de::Deserializer<'de, Error = E>,
		E: serde::de::Error + 'static;

	fn transcode_value<S>(&mut self, value: S) -> Result<(), Box<dyn error::Error>>
	where
		S: serde::ser::Serialize;
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
	fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<(), Box<dyn error::Error>>
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

	fn transcode_value<S>(&mut self, value: S) -> Result<(), Box<dyn error::Error>>
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
	/// This format supports multi-document translation, but does not support
	/// streaming input.
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
