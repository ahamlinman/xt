use std::error::Error;
use std::fmt;
use std::fs::File;
use std::io::{self, BufWriter, Write};
use std::path::PathBuf;
use std::process;
use std::str::{self, FromStr};

use clap::ErrorKind::{HelpDisplayed, VersionDisplayed};
use structopt::StructOpt;

mod detect;
mod input;
mod json;
mod msgpack;
mod toml;
mod transcode;
mod yaml;

use input::{Input, InputRef};

fn main() {
  let opt = match Opt::from_args_safe() {
    Ok(opt) => opt,
    Err(err) => match err.kind {
      HelpDisplayed | VersionDisplayed => err.exit(),
      _ => {
        // As of this writing, clap's error messages (other than those above)
        // include an "error:" prefix, so this gives us consistent formatting
        // for both argument and translation errors. It is a bit fragile, since
        // it's unlikely that clap's error message format is guaranteed to be
        // stable.
        eprint!("jyt {}\n", err.message);
        process::exit(1);
      }
    },
  };

  macro_rules! jyt_exit {
    ($x:expr) => {
      jyt_exit!("{}", $x)
    };
    ($fmt:literal, $($x:expr),*) => {{
      eprint!(concat!("jyt error: ", $fmt, "\n"), $($x),*);
      process::exit(1);
    }};
  }

  let mut output = BufWriter::new(io::stdout());
  if atty::is(atty::Stream::Stdout) && !opt.to.is_safe_for_terminal() {
    jyt_exit!("refusing to output {} to a terminal", opt.to);
  }

  let input = match opt.input() {
    Ok(input) => input,
    Err(err) => jyt_exit!(err),
  };

  let jyt_err = jyt(input, opt.detect_from(), opt.to, &mut output);

  // Some of our serializers, particularly the YAML serializer, don't expose
  // underlying I/O errors in their error chain when they occur. This check
  // gives us a more direct indication of errors related to writing output,
  // particularly broken pipe errors which we'd prefer to hide.
  if let Err(err) = output.flush() {
    match is_broken_pipe(&err) {
      true => return,
      false => jyt_exit!(err),
    }
  }

  if let Err(err) = jyt_err {
    jyt_exit!(err);
  }
}

fn is_broken_pipe(err: &(dyn Error + 'static)) -> bool {
  use io::ErrorKind::BrokenPipe;

  let mut next = Some(err);
  while let Some(err) = next {
    match err.downcast_ref::<io::Error>() {
      Some(err) if err.kind() == BrokenPipe => return true,
      _ => {
        next = err.source();
      }
    }
  }

  false
}

fn jyt<W>(
  mut input: InputRef,
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
      None => Err("cannot parse input as any known format")?,
    },
  };

  match to {
    Format::Json => transcode_input(input, from, json::Output::new(output)),
    Format::Yaml => transcode_input(input, from, yaml::Output::new(output)),
    Format::Toml => transcode_input(input, from, toml::Output::new(output)),
    Format::Msgpack => transcode_input(input, from, msgpack::Output::new(output)),
  }
}

fn transcode_input<O>(input: InputRef, from: Format, output: O) -> Result<(), Box<dyn Error>>
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
  fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<(), Box<dyn Error + 'static>>
  where
    D: serde::de::Deserializer<'de, Error = E>,
    E: serde::de::Error + 'static;

  fn transcode_value<S>(&mut self, value: S) -> Result<(), Box<dyn Error + 'static>>
  where
    S: serde::ser::Serialize;
}

#[derive(StructOpt)]
#[structopt(verbatim_doc_comment)]
/// Translate between serialized data formats
///
/// This version of jyt supports the following formats, each of which may be
/// specified by full name or first character (e.g. '-ty' == '-t yaml'):
///
///      json  Multi-document with self-delineating values (object, array,
///            string) or whitespace between values. Default format for .json
///            files. Supports streaming input.
///
///      yaml  Multi-document with "---" syntax. Default format for .yaml and
///            .yml files.
///
///      toml  Single documents only. Default format for .toml files.
///
///   msgpack  Multi-document as values are naturally self-delineating. Default
///            format for .msgpack files. Supports streaming input.
///
/// Some multi-document input formats can translate a stream of documents
/// without buffering all input into memory first. The input format must be
/// known in advance to enable streaming, usually with an explicit -f.
///
/// When the input format is not known in advance with an explicit -f or file
/// extension, jyt will attempt to auto-detect it by buffering all input into
/// memory and running an unspecified algorithm that is subject to change.
///
/// jyt does not guarantee that every translation is possible, or lossless, or
/// reversible. jyt's behavior is undefined if an input file is modified while
/// running. jyt is not designed for use with untrusted input.
struct Opt {
  #[structopt(short = "t", help = "Format to convert to", default_value = "json")]
  to: Format,

  #[structopt(short = "f", help = "Format to convert from")]
  from: Option<Format>,

  #[structopt(
    name = "file",
    help = "File to read input from [default: stdin]",
    parse(from_os_str)
  )]
  input_filename: Option<PathBuf>,
}

impl Opt {
  fn detect_from(&self) -> Option<Format> {
    if self.from.is_some() {
      return self.from;
    }
    match &self.input_filename {
      None => None,
      Some(path) => match path.extension().map(|ext| ext.to_str()).flatten() {
        Some("json") => Some(Format::Json),
        Some("yaml" | "yml") => Some(Format::Yaml),
        Some("toml") => Some(Format::Toml),
        Some("msgpack") => Some(Format::Msgpack),
        _ => None,
      },
    }
  }

  fn input(&self) -> io::Result<InputRef> {
    match &self.input_filename {
      None => Ok(InputRef::from_reader(io::stdin())),
      Some(path) if path.to_str() == Some("-") => Ok(InputRef::from_reader(io::stdin())),
      Some(path) => {
        let file = File::open(&path)?;
        // Safety: Modification of the mapped file outside the process triggers
        // undefined behavior. Our dirty "solution" is to document this in the
        // help output.
        match unsafe { memmap2::MmapOptions::new().populate().map(&file) } {
          // Per memmap2 docs, it's safe to drop file once mmap succeeds.
          Ok(map) => Ok(InputRef::from_buffer(map)),
          // Fall back to using a reader, in case the file is actually something
          // like a named pipe.
          Err(_) => Ok(InputRef::from_reader(file)),
        }
      }
    }
  }
}

#[derive(Copy, Clone)]
enum Format {
  Json,
  Yaml,
  Toml,
  Msgpack,
}

impl Format {
  fn is_safe_for_terminal(&self) -> bool {
    match self {
      Self::Msgpack => false,
      _ => true,
    }
  }
}

impl FromStr for Format {
  type Err = String;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "j" | "json" => Ok(Self::Json),
      "y" | "yaml" => Ok(Self::Yaml),
      "t" | "toml" => Ok(Self::Toml),
      "m" | "msgpack" => Ok(Self::Msgpack),
      _ => Err(format!("'{}' is not a valid format", s)),
    }
  }
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
