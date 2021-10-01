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

use input::Input;

pub use input::InputHandle;

/// Translates serialized input to serialized output in a different format.
///
/// When `from` is `None`, jyt will attempt to detect the input format using an
/// unspecified and unstable algorithm.
pub fn jyt<'a, W>(
  mut input: InputHandle<'a>,
  from: Option<Format>,
  to: Format,
  output: W,
) -> Result<(), Box<dyn Error>>
where
  W: Write,
{
  let from = match from {
    Some(format) => format,
    None => match detect::detect_format(input.try_clone()?)? {
      Some(format) => format,
      None => return Err("cannot parse input as any known format".into()),
    },
  };

  match to {
    Format::Json => transcode_input(input, from, json::Output::new(output)),
    Format::Yaml => transcode_input(input, from, yaml::Output::new(output)),
    Format::Toml => transcode_input(input, from, toml::Output::new(output)),
    Format::Msgpack => transcode_input(input, from, msgpack::Output::new(output)),
  }
}

fn transcode_input<O>(input: InputHandle, from: Format, output: O) -> Result<(), Box<dyn Error>>
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

trait Output {
  fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<(), Box<dyn Error>>
  where
    D: serde::de::Deserializer<'de, Error = E>,
    E: serde::de::Error + 'static;

  fn transcode_value<S>(&mut self, value: S) -> Result<(), Box<dyn Error>>
  where
    S: serde::ser::Serialize;
}

/// The set of input and output formats supported by jyt.
#[derive(Copy, Clone)]
pub enum Format {
  Json,
  Yaml,
  Toml,
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
