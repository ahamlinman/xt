use std::error::Error;
use std::fs::File;
use std::io::{self, BufWriter, Write};
use std::path::PathBuf;
use std::process;
use std::str::{self, FromStr};

use clap::ErrorKind::{HelpDisplayed, VersionDisplayed};
use memmap2::MmapOptions;
use serde::Deserialize;
use structopt::StructOpt;

mod input;
mod json;
mod msgpack;
mod toml;
mod yaml;

use input::{Input, InputRef};

fn main() {
  let opt = match Opt::from_args_safe() {
    Ok(opt) => opt,
    Err(err) => match err.kind {
      HelpDisplayed | VersionDisplayed => err.exit(),
      _ => {
        // As of this writing, clap's error messages (other than those above)
        // include an "error:" prefix, so this gives consistent formatting for
        // both argument and translation errors. It is a bit fragile, since it's
        // unlikely that clap's error message format is guaranteed to be stable.
        eprint!("jyt {}\n", err.message);
        process::exit(1);
      }
    },
  };

  macro_rules! jyt_exit {
    ($err:tt) => {{
      eprint!("jyt error: {}\n", $err);
      process::exit(1);
    }};
  }

  let input = match opt.input() {
    Ok(input) => input,
    Err(err) => jyt_exit!(err),
  };

  match jyt(input, opt.detect_from(), opt.to, io::stdout()) {
    Ok(_) => {}
    Err(err) if is_broken_pipe(err.as_ref()) => {}
    Err(err) => jyt_exit!(err),
  }
}

fn is_broken_pipe(err: &(dyn Error + 'static)) -> bool {
  return matches!(
    err.downcast_ref::<io::Error>(),
    Some(ioerr) if ioerr.kind() == io::ErrorKind::BrokenPipe
  );
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
    None => match detect_format(input.try_clone()?)? {
      Some(format) => format,
      None => Err("cannot parse input as any known format")?,
    },
  };

  // Note that BufWriter attempts to flush when dropped, but ignores flush
  // errors. This is fine, we only drop before flushing if a transcode error
  // forces us to abort early, in which case the real error happened during
  // transcoding.
  let mut w = BufWriter::new(output);
  match to {
    Format::Json => transcode_input(input, from, json::Output::new(&mut w))?,
    Format::Yaml => transcode_input(input, from, yaml::Output::new(&mut w))?,
    Format::Toml => transcode_input(input, from, toml::Output::new(&mut w))?,
    Format::Msgpack => {
      if atty::is(atty::Stream::Stdout) {
        Err("refusing to output MessagePack to a terminal")?;
      }
      transcode_input(input, from, msgpack::Output::new(&mut w))?
    }
  };
  w.flush()?;
  Ok(())
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

fn detect_format(mut input: InputRef) -> io::Result<Option<Format>> {
  // JSON comes first as it is relatively restrictive compared to the other
  // formats. For example, a "#" comment at the start of a doc could be TOML or
  // YAML, but definitely not JSON, so we can abort parsing fairly early.
  //
  // TOML comes next for a surprising reason… the YAML parser will sometimes
  // accept a TOML file, parsing its contents as a giant string! I'm not sure
  // I'll ever want to understand YAML well enough to explain this. Cargo.lock
  // generally exhibits the behavior, if you're curious.
  //
  // YAML rounds out the text-based formats to help match the behavior of older
  // versions of jyt, which always used YAML as the fallback for unknown input
  // types (I guess if you really do want the giant string behavior).
  for from in [Format::Json, Format::Toml, Format::Yaml] {
    if let Ok(_) = transcode_input(input.try_clone()?, from, DiscardOutput) {
      return Ok(Some(from));
    }
  }

  // In MessagePack, any byte below 0x80 represents a literal unsigned integer.
  // That means any ASCII text input is effectively a valid multi-document
  // MessagePack stream, where every "document" is practically meaningless. To
  // prevent these kinds of weird matches, we only attempt to auto-detect
  // MessagePack when the first byte of input indicates that the next value will
  // be a map or array. Arbitrary non-ASCII input that happens to match one of
  // these markers (e.g. certain UTF-8 multibyte sequences) is extremely
  // unlikely to be a valid sequence of MessagePack values.
  use rmp::Marker::*;
  match input.try_buffer()?.get(0).map(|b| rmp::Marker::from_u8(*b)) {
    Some(FixArray(_) | Array16 | Array32 | FixMap(_) | Map16 | Map32) => {
      if let Ok(_) = transcode_input(input, Format::Msgpack, DiscardOutput) {
        return Ok(Some(Format::Msgpack));
      }
    }
    _ => {}
  }

  Ok(None)
}

struct DiscardOutput;

impl Output for DiscardOutput {
  fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<(), Box<dyn Error>>
  where
    D: serde::de::Deserializer<'de, Error = E>,
    E: serde::de::Error + 'static,
  {
    match serde::de::IgnoredAny::deserialize(de) {
      Ok(_) => Ok(()),
      Err(err) => Err(err)?,
    }
  }

  fn transcode_value<S>(&mut self, value: S) -> Result<(), Box<dyn Error>>
  where
    S: serde::ser::Serialize,
  {
    // Picking MessagePack as it's probably the fastest real serializer we have.
    // I would love to have a "discard" serializer of some sort but I haven't
    // found one yet.
    value.serialize(&mut rmp_serde::Serializer::new(io::sink()))?;
    Ok(())
  }
}

#[derive(StructOpt)]
#[structopt(verbatim_doc_comment)]
/// Translate between serialized data formats
///
/// This version of jyt supports the following formats, each of which may be
/// specified by full name or first character (e.g. '-ty' == '-t yaml'):
///
///      json  Multi-document with self-delineating values (object, array,
///            string) and / or whitespace between values. Default format for
///            .json files. Supports streaming input.
///
///      yaml  Multi-document with "---" syntax. Default format for .yaml and
///            .yml files.
///
///      toml  Single documents only. Default format for .toml files.
///
///   msgpack  Multi-document as values are naturally self-delineating. Supports
///            streaming input.
///
/// Some multi-document input formats can translate a stream of documents
/// without buffering all input into memory first. The input format must be
/// known in advance, usually with an explicit -f.
///
/// When the input format is not known in advance with an explicit -f or file
/// extension detection, jyt will attempt to auto-detect it by buffering all
/// input into memory and running an unspecified algorithm that is subject to
/// change over time.
///
/// jyt does not guarantee that every conversion is possible, or lossless, or
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
        match unsafe { MmapOptions::new().populate().map(&file) } {
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
