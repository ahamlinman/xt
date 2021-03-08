use std::error::Error;
use std::fs::File;
use std::io::{self, Read, Write};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::process;
use std::str::{self, FromStr};

use memmap2::Mmap;
use serde::Deserialize;
use structopt::StructOpt;

fn main() {
  let opt = Opt::from_args();

  if let Err(e) = jyt(opt) {
    match e.downcast_ref::<io::Error>() {
      Some(e) if e.kind() == io::ErrorKind::BrokenPipe => return,
      _ => {}
    }
    eprint!("jyt error: {}\n", e);
    process::exit(1);
  }
}

fn jyt(opt: Opt) -> Result<(), Box<dyn Error>> {
  match opt.detect_from() {
    None | Some(Format::Yaml) => {
      let reader = get_input_reader(opt.input_file)?;
      let de = serde_yaml::Deserializer::from_reader(reader);
      transcode_to(de, opt.to)?;
    }
    Some(Format::Json) => {
      let reader = get_input_reader(opt.input_file)?;
      let mut de = serde_json::Deserializer::from_reader(reader);
      transcode_to(&mut de, opt.to)?;
    }
    Some(Format::Toml) => {
      let slice = get_input_slice(opt.input_file)?;
      let input_str = str::from_utf8(&slice)?;
      let mut de = toml::Deserializer::new(input_str);
      transcode_to(&mut de, opt.to)?;
    }
  }
  Ok(())
}

fn get_input_reader<P>(path: Option<P>) -> io::Result<Box<dyn Read>>
where
  P: AsRef<Path>,
{
  match filter_stdin_path(path) {
    None => Ok(Box::new(io::stdin())),
    Some(p) => Ok(Box::new(File::open(p)?)),
  }
}

fn get_input_slice<P>(path: Option<P>) -> Result<Box<dyn Deref<Target = [u8]>>, Box<dyn Error>>
where
  P: AsRef<Path>,
{
  let mut input: Box<dyn Read> = match filter_stdin_path(path) {
    None => Box::new(io::stdin()),
    Some(p) => {
      // mmap the file, or fall back to standard reading if it fails. As dirty
      // as it is, we document for users that modifying the input while it's
      // mapped results in undefined behavior.
      let file = File::open(p)?;
      match unsafe { Mmap::map(&file) } {
        Ok(map) => return Ok(Box::new(map)),
        Err(_) => Box::new(file),
      }
    }
  };

  let mut buf = Vec::new();
  input.read_to_end(&mut buf)?;
  Ok(Box::new(buf))
}

fn filter_stdin_path<P>(path: Option<P>) -> Option<P>
where
  P: AsRef<Path>,
{
  path.filter(|p| p.as_ref().to_str() != Some("-"))
}

fn transcode_to<'a, D>(de: D, to: Format) -> Result<(), Box<dyn Error>>
where
  D: serde::de::Deserializer<'a>,
{
  match to {
    Format::Json if atty::is(atty::Stream::Stdout) => {
      let mut ser = serde_json::Serializer::pretty(io::stdout());
      serde_transcode::transcode(de, &mut ser)?;
      println!(""); // Extra newline so it looks even prettier
    }
    Format::Json => {
      let mut ser = serde_json::Serializer::new(io::stdout());
      serde_transcode::transcode(de, &mut ser)?;
    }
    Format::Yaml => {
      let mut ser = serde_yaml::Serializer::new(io::stdout());
      serde_transcode::transcode(de, &mut ser)?;
    }
    Format::Toml => {
      // TOML requires that all non-table values appear before any tables at a
      // given "level." We can't enforce that JSON and YAML inputs put all
      // objects / maps before other types, so instead of the normal transcode
      // workflow we buffer these inputs into a toml::Value, which will
      // serialize them back out in the necessary order.
      //
      // The error type here is bound by the lifetime of the deserializer;
      // converting to a string allows us to maintain 'static on the error this
      // function returns.
      let value = toml::Value::deserialize(de).map_err(|e| e.to_string())?;

      // TODO: Write directly to stdout if / when the toml crate gains support.
      let output_buf = toml::to_string(&value)?;
      io::stdout().write(output_buf.as_bytes())?;
    }
  }
  Ok(())
}

#[derive(StructOpt)]
#[structopt(verbatim_doc_comment)]
/// Translate between serialized data formats
///
/// Supported formats are: json, yaml, and toml. Formats can be specified using
/// the first character of the name, e.g. '-ty' is the same as '-t yaml'.
///
/// When reading from a file, jyt will try to detect a value for -f based on the
/// extension. Otherwise, jyt will default to '-f yaml', which supports both YAML
/// and JSON input. To read TOML from stdin, specify '-f toml' (a.k.a. '-ft')
/// explicitly.
///
/// jyt reads JSON and YAML input in streaming fashion. When reading TOML from
/// stdin, jyt buffers the full input into memory. When reading TOML from a file,
/// jyt will attempt to map the file directly into memory (buffering only if the
/// map fails). Modifying a mapped input file while jyt is running will trigger
/// undefined behavior.
///
/// jyt writes JSON and YAML output in streaming fashion, and outputs values in
/// the same order as the input. With JSON output, jyt pretty-prints if
/// outputting to a terminal, and prints compactly otherwise. With TOML output,
/// jyt fully parses the input into memory, then sorts non-table values before
/// tables in the output to ensure it is valid TOML.
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
  input_file: Option<PathBuf>,
}

impl Opt {
  fn detect_from(&self) -> Option<Format> {
    if self.from.is_some() {
      return self.from.clone();
    }

    match &self.input_file {
      None => None,
      Some(p) => match p.extension().map(|ext| ext.to_str()).flatten() {
        Some("json") => Some(Format::Json),
        Some("yaml") | Some("yml") => Some(Format::Yaml),
        Some("toml") => Some(Format::Toml),
        _ => None,
      },
    }
  }
}

#[derive(Clone)]
enum Format {
  Json,
  Yaml,
  Toml,
}

impl FromStr for Format {
  type Err = &'static str;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "j" | "json" => Ok(Self::Json),
      "y" | "yaml" => Ok(Self::Yaml),
      "t" | "toml" => Ok(Self::Toml),
      _ => Err("unknown format"),
    }
  }
}
