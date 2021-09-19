use std::convert::{TryFrom, TryInto};
use std::error::Error;
use std::fs::File;
use std::io::{self, BufWriter, Read, Write};
use std::ops::Deref;
use std::path::PathBuf;
use std::process;
use std::rc::Rc;
use std::str::{self, FromStr};

use clap::ErrorKind::{HelpDisplayed, VersionDisplayed};
use memmap2::MmapOptions;
use serde::Deserialize;
use structopt::StructOpt;

mod json;
mod msgpack;
mod toml;
mod yaml;

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

  match jyt(opt) {
    Ok(_) => {}
    Err(err) if is_broken_pipe(err.as_ref()) => {}
    Err(err) => {
      eprint!("jyt error: {}\n", err);
      process::exit(1);
    }
  }
}

fn is_broken_pipe(err: &(dyn Error + 'static)) -> bool {
  return matches!(
    err.downcast_ref::<io::Error>(),
    Some(ioerr) if ioerr.kind() == io::ErrorKind::BrokenPipe
  );
}

fn jyt(opt: Opt) -> Result<(), Box<dyn Error>> {
  let mut input: Input = opt.input_opt().try_into()?;
  let from = match opt.detect_from() {
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
  let mut w = BufWriter::new(io::stdout());

  match opt.to {
    Format::Json => {
      let output = json::Output::new(&mut w);
      transcode_input(input, from, output)?;
    }
    Format::Yaml => {
      let output = yaml::Output::new(&mut w);
      transcode_input(input, from, output)?;
    }
    Format::Toml => {
      let output = toml::Output::new(&mut w);
      transcode_input(input, from, output)?;
    }
    Format::Msgpack => {
      if atty::is(atty::Stream::Stdout) {
        Err("refusing to output MessagePack to a terminal")?;
      }
      let output = msgpack::Output::new(&mut w);
      transcode_input(input, from, output)?;
    }
  }

  w.flush()?;
  Ok(())
}

fn transcode_input<T>(input: Input, from: Format, output: T) -> Result<(), Box<dyn Error>>
where
  T: TranscodeFrom,
{
  match from {
    Format::Json => json::transcode(input, output),
    Format::Yaml => yaml::transcode(input, output),
    Format::Toml => toml::transcode(input, output),
    Format::Msgpack => msgpack::transcode(input, output),
  }
}

trait TranscodeFrom {
  fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<(), Box<dyn Error + 'static>>
  where
    D: serde::de::Deserializer<'de, Error = E>,
    E: serde::de::Error + 'static;
}

enum Input {
  Buffered(Rc<dyn Deref<Target = [u8]>>),
  Unbuffered(Box<dyn Read>),
}

impl Input {
  fn try_buffer(&mut self) -> io::Result<&(dyn Deref<Target = [u8]>)> {
    *self = match self {
      Self::Buffered(ref buf) => return Ok(buf.as_ref()),
      Self::Unbuffered(r) => {
        let mut buf = Vec::new();
        r.read_to_end(&mut buf)?;
        Self::Buffered(Rc::new(buf))
      }
    };
    self.try_buffer()
  }

  fn try_clone(&mut self) -> io::Result<Input> {
    *self = match self {
      Self::Buffered(buf) => return Ok(Self::Buffered(Rc::clone(buf))),
      Self::Unbuffered(r) => {
        let mut buf = Vec::new();
        r.read_to_end(&mut buf)?;
        Self::Buffered(Rc::new(buf))
      }
    };
    self.try_clone()
  }
}

impl TryFrom<InputOpt<'_>> for Input {
  type Error = io::Error;

  fn try_from(input_opt: InputOpt) -> io::Result<Input> {
    match input_opt {
      InputOpt::Stdin => Ok(Self::Unbuffered(Box::new(io::stdin()))),
      InputOpt::File(path) => {
        let file = File::open(path)?;
        // Safety: Modification of the mapped file outside the process triggers
        // undefined behavior. Our dirty "solution" is to document this in the
        // help output.
        match unsafe { MmapOptions::new().populate().map(&file) } {
          // Per memmap2 docs, it's safe to drop the file once mmap succeeds.
          Ok(map) => return Ok(Self::Buffered(Rc::new(map))),
          // If mmap fails, we can still try regular buffering.
          Err(_) => Ok(Self::Unbuffered(Box::new(file))),
        }
      }
    }
  }
}

fn detect_format(mut input: Input) -> io::Result<Option<Format>> {
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
  match input.try_buffer()?.get(0).map(|b| rmp::Marker::from_u8(*b)) {
    Some(
      rmp::Marker::FixArray(_)
      | rmp::Marker::Array16
      | rmp::Marker::Array32
      | rmp::Marker::FixMap(_)
      | rmp::Marker::Map16
      | rmp::Marker::Map32,
    ) => {
      if let Ok(_) = transcode_input(input, Format::Msgpack, DiscardOutput) {
        return Ok(Some(Format::Msgpack));
      }
    }
    _ => {}
  }

  Ok(None)
}

struct DiscardOutput;

impl TranscodeFrom for DiscardOutput {
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
///            .json files.
///
///      yaml  Multi-document with "---" syntax. Default format for .yaml and
///            .yml files.
///
///      toml  Single documents only. Default format for .toml files.
///
///   msgpack  Multi-document as values are naturally self-delineating.
///
/// When the input format is not specified with -f or detected from a file
/// extension, jyt will attempt to auto-detect it from the input using an
/// unspecified algorithm that is subject to change over time.
///
/// jyt does not guarantee that every conversion is possible, or lossless, or
/// reversible. jyt's behavior is undefined if an input file is modified while
/// running.
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

  fn input_opt(&self) -> InputOpt {
    match &self.input_filename {
      None => InputOpt::Stdin,
      Some(path) if path.to_str() == Some("-") => InputOpt::Stdin,
      Some(path) => InputOpt::File(path),
    }
  }
}

enum InputOpt<'p> {
  Stdin,
  File(&'p PathBuf),
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
