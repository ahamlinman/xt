use std::error::Error;
use std::fs::File;
use std::io::{self, BufWriter, Read, Write};
use std::ops::Deref;
use std::path::PathBuf;
use std::process;
use std::str::{self, FromStr};

use memmap2::MmapOptions;
use serde::Deserialize;
use structopt::StructOpt;

fn main() {
  let opt = match Opt::from_args_safe() {
    Ok(opt) => opt,
    Err(err) => match err.kind {
      clap::ErrorKind::HelpDisplayed | clap::ErrorKind::VersionDisplayed => err.exit(),
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

  if let Err(err) = jyt(opt) {
    if is_broken_pipe(err.as_ref()) {
      return;
    }
    eprint!("jyt error: {}\n", err);
    process::exit(1);
  }
}

fn is_broken_pipe(err: &(dyn Error + 'static)) -> bool {
  return matches!(
    err.downcast_ref::<io::Error>(),
    Some(ioerr) if ioerr.kind() == io::ErrorKind::BrokenPipe
  );
}

fn jyt(opt: Opt) -> Result<(), Box<dyn Error>> {
  // serde_json implements a from_reader method, however with file input it is
  // significantly slower than reading from a mmap'ed slice, and with stdin it
  // seems to be no better (time or memory wise) than full buffering. serde_yaml
  // also implements a from_reader method, but as of this writing it simply
  // buffers the reader into a byte vector and defers to from_slice. TL;DR
  // there's no benefit to anything other than slice input.
  let input = get_input_slice(opt.input_source())?;

  // Note that BufWriter attempts to flush when dropped, but ignores flush
  // errors. This is fine, we only drop before flushing if a transcode error
  // forces us to abort early, in which case the real error happened during
  // transcoding.
  let mut w = BufWriter::new(io::stdout());
  let from = opt.detect_from().unwrap_or(Format::Yaml);

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
      // mmap the file to represent it directly as a slice, or fall back to
      // standard buffering if that fails.
      //
      // This is marked unsafe as modifying a mapped file outside of the process
      // can produce undefined behavior. Our dirty "solution" is to document
      // this for users.
      let file = File::open(path)?;
      match unsafe { MmapOptions::new().populate().map(&file) } {
        Ok(map) => return Ok(Box::new(map)),
        Err(_) => Box::new(file),
      }
    }
  };

  let mut buf = Vec::new();
  input.read_to_end(&mut buf)?;
  Ok(Box::new(buf))
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
  fn transcode_from<'de, D>(&mut self, de: D) -> Result<(), Box<dyn Error>>
  where
    D: serde::de::Deserializer<'de>;
}

struct JsonOutput<W>(W);

impl<W> Output for JsonOutput<W>
where
  W: Write,
{
  fn transcode_from<'de, D>(&mut self, de: D) -> Result<(), Box<dyn Error>>
  where
    D: serde::de::Deserializer<'de>,
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
  fn transcode_from<'de, D>(&mut self, de: D) -> Result<(), Box<dyn Error>>
  where
    D: serde::de::Deserializer<'de>,
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
  fn transcode_from<'de, D>(&mut self, de: D) -> Result<(), Box<dyn Error>>
  where
    D: serde::de::Deserializer<'de>,
  {
    self.used = match self.used {
      false => true,
      true => Err("toml does not support multi-document output")?,
    };

    // TOML requires that all non-table values appear before any tables at a
    // given "level." Since we can't enforce this for all input types, we buffer
    // the inputs into a toml::Value, which will serialize them back out in the
    // necessary order.
    //
    // The error type here is bound by the deserializer's lifetime. Converting
    // to a string allows us to maintain 'static bounds on the error this
    // function returns.
    let value = toml::Value::deserialize(de).map_err(|e| e.to_string())?;

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
/// This version of jyt supports the following formats for input and output,
/// each of which may be specified by full name or first character
/// (e.g. '-ty' == '-t yaml'):
///
///   json: Multi-document with self-delineating values (object, array, string)
///         and / or whitespace between values.
///
///   yaml: Multi-document with "---" syntax.
///
///   toml: Single documents only. Does not support null values.
///
/// With file inputs, jyt will try to detect the input format based on file
/// extensions. Otherwise it defaults to '-f yaml', which is also compatible
/// with single-document JSON input (but slower than '-f json'). jyt's behavior
/// is undefined if an input file is modified while jyt is running.
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
        Some("yaml") | Some("yml") => Some(Format::Yaml),
        Some("toml") => Some(Format::Toml),
        _ => None,
      },
    }
  }

  fn input_source(&self) -> InputSource {
    match &self.input_filename {
      None => InputSource::Stdin,
      Some(path) => {
        if path.to_str() == Some("-") {
          InputSource::Stdin
        } else {
          InputSource::File(path)
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
