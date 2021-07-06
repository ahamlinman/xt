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
  // Formats are organized, in rough terms, from least to most "permissive."
  // It's important that YAML be last, since it seems like just about any input
  // that doesn't contain ":" can be parsed as a YAML string. This also matches
  // the behavior of older versions of jyt that always used YAML as the fallback
  // for unknown input types.
  for from in [Format::Json, Format::Toml, Format::Yaml] {
    if let Ok(_) = transcode_all_input(input, from, DiscardOutput) {
      return Some(from);
    }
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

#[derive(StructOpt)]
#[structopt(verbatim_doc_comment)]
/// Translate between serialized data formats
///
/// This version of jyt supports the following formats, each of which may be
/// specified by full name or first character (e.g. '-ty' == '-t yaml'):
///
///   json: Multi-document with self-delineating values (object, array, string)
///         and / or whitespace between values. Default format for .json files.
///
///   yaml: Multi-document with "---" syntax. Default format for .yaml and .yml
///         files.
///
///   toml: Single documents only. Does not support null values. Default format
///         for .toml files.
///
/// When the input format is not specified with -f or detected from a file
/// extension, jyt will attempt to auto-detect it by parsing the input as
/// different formats in an unspecified order until one works. jyt's behavior is
/// undefined if an input file is modified while jyt is running.
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
}

impl FromStr for Format {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "j" | "json" => Ok(Self::Json),
      "y" | "yaml" => Ok(Self::Yaml),
      "t" | "toml" => Ok(Self::Toml),
      _ => Err(format!("'{}' is not a valid format", s)),
    }
  }
}

enum InputSource<'p> {
  Stdin,
  File(&'p PathBuf),
}
