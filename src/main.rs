use std::fs::File;
use std::io::{self, Read, Write};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use serde::Deserialize;
use structopt::StructOpt;

fn main() {
  let opt = Opt::from_args();
  match opt.detect_from() {
    None | Some(Format::Yaml) => {
      let reader = get_reader(opt.input_file);
      let de = serde_yaml::Deserializer::from_reader(reader);
      transcode_to(de, opt.to);
    }
    Some(Format::Json) => {
      let reader = get_reader(opt.input_file);
      let mut de = serde_json::Deserializer::from_reader(reader);
      transcode_to(&mut de, opt.to);
    }
    Some(Format::Toml) => {
      let slice = get_slice(opt.input_file);
      let input_str = std::str::from_utf8(&slice).unwrap();
      let mut de = toml::Deserializer::new(input_str);
      transcode_to(&mut de, opt.to);
    }
  }
}

fn get_reader<P: AsRef<Path>>(path: Option<P>) -> Box<dyn Read> {
  match path.filter(|p| p.as_ref().to_str() != Some("-")) {
    Some(p) => Box::new(File::open(p).expect("failed to open file")),
    None => Box::new(io::stdin()),
  }
}

fn get_slice<P: AsRef<Path>>(path: Option<P>) -> Box<dyn Deref<Target = [u8]>> {
  match path.filter(|p| p.as_ref().to_str() != Some("-")) {
    Some(p) => {
      // TODO: something about the fact this is unsafe?
      // truncating the file while it's mapped => undefined behavior
      let map = {
        let f = File::open(p).unwrap();
        unsafe { memmap2::Mmap::map(&f).unwrap() }
      };
      Box::new(map)
    }
    None => {
      let mut buf = Vec::new();
      io::stdin().read_to_end(&mut buf).unwrap();
      Box::new(buf)
    }
  }
}

fn transcode_to<'a, D>(de: D, to: Format)
where
  D: serde::de::Deserializer<'a>,
{
  match to {
    Format::Json if atty::is(atty::Stream::Stdout) => {
      let mut ser = serde_json::Serializer::pretty(io::stdout());
      serde_transcode::transcode(de, &mut ser).expect("failed to serialize JSON");
      println!(""); // Extra newline so it looks even prettier
    }
    Format::Json => {
      let mut ser = serde_json::Serializer::new(io::stdout());
      serde_transcode::transcode(de, &mut ser).expect("failed to serialize JSON");
    }
    Format::Yaml => {
      let mut ser = serde_yaml::Serializer::new(io::stdout());
      serde_transcode::transcode(de, &mut ser).expect("failed to serialize YAML");
    }
    Format::Toml => {
      // TOML requires that all non-table values appear before any tables at a
      // given "level." We can't enforce that JSON and YAML inputs put all
      // objects / maps before other types, so instead of the normal transcode
      // workflow we buffer these inputs into a toml::Value, which will
      // serialize them back out in the necessary order.
      let value = toml::Value::deserialize(de).expect("failed to serialize TOML");

      // TODO: Write directly to output if the toml crate gains support for it.
      let output_buf = toml::to_string(&value).expect("failed to serialize TOML");
      io::stdout().write(output_buf.as_bytes()).unwrap();
    }
  };
}

#[derive(StructOpt)]
#[structopt(verbatim_doc_comment)]
/// Translate between serialized data formats
///
/// Supported formats are json, yaml, and toml.
///
/// When reading from a file, jyt will try to detect a value for -f based on the
/// extension. Otherwise, jyt will default to '-f yaml', which supports both YAML
/// and JSON input. To read TOML from stdin, specify '-f toml' explicitly.
///
/// jyt reads JSON and YAML input in streaming fashion. When reading TOML from
/// stdin, jyt buffers the full input into memory. When reading TOML from a file,
/// jyt will mmap the file. Modifying the mmap'ed file while jyt is running will
/// probably cause issues.
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
      Some(p) => match p.extension().map(|s| s.to_str()).flatten() {
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
      "json" => Ok(Self::Json),
      "yaml" => Ok(Self::Yaml),
      "toml" => Ok(Self::Toml),
      _ => Err("unknown format"),
    }
  }
}
