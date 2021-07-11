use std::error::Error;
use std::fs::File;
use std::io::{self, BufWriter, Read, Write};
use std::ops::Deref;
use std::path::PathBuf;
use std::process;
use std::str::{self, FromStr};

use clap::ErrorKind::{HelpDisplayed, VersionDisplayed};
use memmap2::MmapOptions;
use serde::Deserialize;
use structopt::StructOpt;

mod jyt;

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
  // serde_json and serde_yaml support deserializing from readers rather than
  // slices, however there's no real benefit to doing this. serde_json is much
  // slower with readers, and memory use isn't much different between buffering
  // stdin and streaming it to the parser (presumably it borrows from the input
  // instead of allocating a bunch of stuff?). serde_yaml buffers the contents
  // of the reader into a slice under the hood, so it's no different at all.
  let input = get_input_slice(opt.input_source())?;
  let from = match opt.detect_from() {
    Some(format) => format,
    None => match detect_format(&input) {
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
      let output = JsonOutput(&mut w);
      transcode_all_input(&input, from, output)?;
    }
    Format::Yaml => {
      let output = YamlOutput(&mut w);
      transcode_all_input(&input, from, output)?;
    }
    Format::Toml => {
      let output = TomlOutput {
        w: &mut w,
        used: false,
      };
      transcode_all_input(&input, from, output)?;
    }
    Format::Msgpack => {
      if atty::is(atty::Stream::Stdout) {
        Err("refusing to output msgpack to a terminal")?;
      }
      let output = MsgpackOutput(&mut w);
      transcode_all_input(&input, from, output)?;
    }
  }

  w.flush()?;
  Ok(())
}

fn get_input_slice(source: InputSource) -> io::Result<Box<dyn Deref<Target = [u8]>>> {
  let mut input: Box<dyn Read> = match source {
    InputSource::Stdin => Box::new(io::stdin()),
    InputSource::File(path) => {
      let file = File::open(path)?;
      // Safety: Modification of the mapped file outside the process triggers
      // undefined behavior. Our dirty "solution" is to document this in the
      // help output.
      match unsafe { MmapOptions::new().populate().map(&file) } {
        // Per memmap2 docs, it's safe to drop the file once mmap succeeds.
        Ok(map) => return Ok(Box::new(map)),
        // If mmap fails, we can still try regular buffering.
        Err(_) => Box::new(file),
      }
    }
  };

  let mut buf = Vec::new();
  input.read_to_end(&mut buf)?;
  Ok(Box::new(buf))
}

fn detect_format(input: &[u8]) -> Option<Format> {
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
    if let Ok(_) = transcode_all_input(input, from, DiscardOutput) {
      return Some(from);
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
  match input.get(0).map(|b| rmp::Marker::from_u8(*b)) {
    Some(
      rmp::Marker::FixArray(_)
      | rmp::Marker::Array16
      | rmp::Marker::Array32
      | rmp::Marker::FixMap(_)
      | rmp::Marker::Map16
      | rmp::Marker::Map32,
    ) => {
      if let Ok(_) = transcode_all_input(input, Format::Msgpack, DiscardOutput) {
        return Some(Format::Msgpack);
      }
    }
    _ => {}
  }

  None
}

fn transcode_all_input<O>(input: &[u8], from: Format, mut output: O) -> Result<(), Box<dyn Error>>
where
  O: Output,
{
  match from {
    Format::Json => {
      let mut de = serde_json::Deserializer::from_slice(input);
      while let Err(_) = de.end() {
        output.transcode_from(&mut de)?;
      }
    }
    Format::Yaml => {
      for de in serde_yaml::Deserializer::from_slice(input) {
        output.transcode_from(de)?;
      }
    }
    Format::Toml => {
      let input_str = str::from_utf8(input)?;
      let mut de = toml::Deserializer::new(input_str);
      output.transcode_from(&mut de)?;
    }
    Format::Msgpack => {
      let mut input = input;
      while input.len() > 0 {
        let size = jyt::msgpack::next_value_size(input)?;
        let (next, rest) = input.split_at(size);
        let mut de = rmp_serde::Deserializer::from_read_ref(next);
        output.transcode_from(&mut de)?;
        input = rest;
      }
    }
  }
  Ok(())
}

trait Output {
  fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<(), Box<dyn Error + 'static>>
  where
    D: serde::de::Deserializer<'de, Error = E>,
    E: serde::de::Error + 'static;
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
}

struct JsonOutput<W>(W);

impl<W> Output for JsonOutput<W>
where
  W: Write,
{
  fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<(), Box<dyn Error>>
  where
    D: serde::de::Deserializer<'de, Error = E>,
    E: serde::de::Error + 'static,
  {
    let mut ser = serde_json::Serializer::new(&mut self.0);
    serde_transcode::transcode(de, &mut ser)?;
    writeln!(&mut self.0, "")?;
    Ok(())
  }
}

struct YamlOutput<W>(W);

impl<W> Output for YamlOutput<W>
where
  W: Write,
{
  fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<(), Box<dyn Error>>
  where
    D: serde::de::Deserializer<'de, Error = E>,
    E: serde::de::Error + 'static,
  {
    let mut ser = serde_yaml::Serializer::new(&mut self.0);
    serde_transcode::transcode(de, &mut ser)?;
    Ok(())
  }
}

struct TomlOutput<W> {
  w: W,
  used: bool,
}

impl<W> Output for TomlOutput<W>
where
  W: Write,
{
  fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<(), Box<dyn Error>>
  where
    D: serde::de::Deserializer<'de, Error = E>,
    E: serde::de::Error + 'static,
  {
    self.used = match self.used {
      false => true,
      true => Err("TOML does not support multi-document output")?,
    };

    // TOML requires that all non-table values appear before any tables at a
    // given "level." Since we can't enforce this for all input types, we buffer
    // the inputs into a toml::Value, which will serialize them back out in the
    // necessary order.
    let value = toml::Value::deserialize(de)?;

    // From the spec: "TOML is designed to map unambiguously to a hash table."
    // Without this check, the other input types could produce something like a
    // boolean or array that we would attempt to dump the TOML representation of
    // without a second thought. The toml crate can even produce invalid TOML
    // for some of these representations, such as dumping each element of an
    // array of tables with an empty name, i.e. with a "[[]]" header.
    if !value.is_table() {
      Err("root of TOML output must be a table")?;
    }

    // As of this writing, the toml crate can't output directly to a writer.
    let output_buf = toml::to_string_pretty(&value)?;
    self.w.write_all(output_buf.as_bytes())?;
    Ok(())
  }
}

struct MsgpackOutput<W>(W);

impl<W> Output for MsgpackOutput<W>
where
  W: Write,
{
  fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<(), Box<dyn Error>>
  where
    D: serde::de::Deserializer<'de, Error = E>,
    E: serde::de::Error + 'static,
  {
    let mut ser = rmp_serde::Serializer::new(&mut self.0);
    serde_transcode::transcode(de, &mut ser)?;
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
/// json     Multi-document with self-delineating values (object, array, string)
///          and / or whitespace between values. Default format for .json files.
///
/// yaml     Multi-document with "---" syntax. Default format for .yaml and .yml
///          files.
///
/// toml     Single documents only. Does not support all values supported by
///          other formats. Default format for .toml files.
///
/// msgpack  Multi-document as values are naturally self-delineating.
///
/// When the input format is not specified with -f or detected from a file
/// extension, jyt will attempt to auto-detect it from the input using an
/// unspecified algorithm that is subject to change over time.
///
/// jyt's behavior is undefined if an input file is modified while running.
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

  fn input_source(&self) -> InputSource {
    match &self.input_filename {
      None => InputSource::Stdin,
      Some(path) if path.to_str() == Some("-") => InputSource::Stdin,
      Some(path) => InputSource::File(path),
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

enum InputSource<'p> {
  Stdin,
  File(&'p PathBuf),
}
