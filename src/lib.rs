#![deny(
    clippy::cast_lossless,
    clippy::cast_possible_truncation,
    clippy::enum_glob_use,
    clippy::semicolon_if_nothing_returned
)]

//! The main entrypoint for the xt serialized data translation tool.
//!
//! **This interface is not stable!** To convert between serialized data formats
//! in Rust code, consider [`serde_transcode`](https://docs.rs/serde-transcode)
//! as a more flexible and widely used implementation.

use std::error::Error;
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

/// Translates serialized input to serialized output in a different format.
///
/// When `from` is `None`, xt will attempt to detect the input format using an
/// unspecified and unstable algorithm.
pub fn translate<W>(
    mut input: Handle<'_>,
    from: Option<Format>,
    to: Format,
    output: W,
) -> Result<(), Box<dyn Error>>
where
    W: Write,
{
    let from = match from {
        Some(format) => format,
        None => match detect::detect_format(&mut input)? {
            Some(format) => format,
            None => return Err("unable to detect input format".into()),
        },
    };

    match to {
        Format::Json => transcode_input(input, from, json::Output::new(output)),
        Format::Yaml => transcode_input(input, from, yaml::Output::new(output)),
        Format::Toml => transcode_input(input, from, toml::Output::new(output)),
        Format::Msgpack => transcode_input(input, from, msgpack::Output::new(output)),
    }
}

/// Translates serialized input to a known output format.
///
/// The compiler will monomorphize a copy of this function for each supported
/// output format, giving us n² individually optimized code paths for each
/// possible translation. Unsurprisingly, this is far more performant than a
/// dynamic dispatch approach using [`erased_serde`][erased_serde]. Perhaps more
/// surprisingly, past experiments show that it also generates *smaller* code
/// than the dynamic dispatch approach, especially with link-time optimization
/// enabled. This could change if xt gains support for a larger set of formats.
///
/// [erased_serde]: https://github.com/dtolnay/erased-serde
fn transcode_input<O>(input: Handle, from: Format, output: O) -> Result<(), Box<dyn Error>>
where
    O: Output,
{
    match from {
        Format::Json => json::transcode(input, output),
        Format::Yaml => yaml::transcode(input, output),
        Format::Toml => toml::transcode(input, output),
        Format::Msgpack => msgpack::transcode(input, output),
    }
}

/// A trait for output formats to receive their translatable input.
trait Output {
    fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<(), Box<dyn Error>>
    where
        D: serde::de::Deserializer<'de, Error = E>,
        E: serde::de::Error + 'static;

    fn transcode_value<S>(&mut self, value: S) -> Result<(), Box<dyn Error>>
    where
        S: serde::ser::Serialize;
}

/// The set of input and output formats supported by xt.
///
/// The feature sets of the supported formats vary along two key dimensions.
///
/// Formats supporting **multi-document translation** can represent multiple
/// independent "documents" within a single input stream. For example, xt can
/// translate a single YAML file with documents separated by `---` markers into
/// a stream of newline delimited JSON values. In contrast, single document
/// formats may only represent one logical "document" per input stream. When
/// translating an input with more than one document to a single document output
/// format, xt will return an error as it starts to translate the second
/// document in the stream.
///
/// Formats supporting **streaming input** can translate a multi-document input
/// stream without first buffering the entire stream into memory. This enables
/// translation of unbounded input sources, such as an upstream program emitting
/// an event stream to xt through a pipe. Formats that do not support streaming
/// must buffer the entire input stream into memory before translating the first
/// document.
///
/// Support for each data format comes largely from external Rust crates, with
/// only minor additional preprocessing from xt for select formats. Note that
/// the crate selection for each format is **not stable**, and is documented for
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
