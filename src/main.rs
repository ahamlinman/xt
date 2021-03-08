use std::fs::File;
use std::io::{self, Read, Write};
use std::path::PathBuf;
use std::str::FromStr;

use structopt::StructOpt;

fn main() {
  let opt = Opt::from_args();
  let output = io::stdout();

  match opt.from {
    None | Some(Format::Yaml) => {
      let input = open_reader(opt.input_file);
      let de = serde_yaml::Deserializer::from_reader(input);
      transcode_to(de, opt.to, output);
    }
    Some(Format::Json) => {
      let input = open_reader(opt.input_file);
      let mut de = serde_json::Deserializer::from_reader(input);
      transcode_to(&mut de, opt.to, output);
    }
    Some(Format::Toml) => {
      let mut input = open_reader(opt.input_file);
      let mut s = String::new();
      input.read_to_string(&mut s).unwrap();
      let mut de = toml::Deserializer::new(s.as_str());
      transcode_to(&mut de, opt.to, output);
    }
  }
}

fn open_reader(filename: Option<PathBuf>) -> Box<dyn Read> {
  match filename {
    None => Box::new(io::stdin()),
    Some(p) if p.to_str() == Some("-") => Box::new(io::stdin()),
    Some(p) => Box::new(File::open(p).expect("failed to open file")),
  }
}

fn transcode_to<'a, D, W>(de: D, to: Format, mut output: W)
where
  D: serde::de::Deserializer<'a>,
  W: Write,
{
  match to {
    Format::Json => {
      let mut ser = serde_json::Serializer::pretty(output);
      serde_transcode::transcode(de, &mut ser).expect("failed to serialize JSON")
    }
    Format::Yaml => {
      let mut ser = serde_yaml::Serializer::new(output);
      serde_transcode::transcode(de, &mut ser).expect("failed to serialize YAML")
    }
    Format::Toml => {
      let mut s = String::new();
      let mut ser = toml::Serializer::new(&mut s);
      serde_transcode::transcode(de, &mut ser).expect("failed to serialize TOML");
      output.write(s.as_bytes()).unwrap();
    }
  };
}

#[derive(StructOpt)]
#[structopt(about)]
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
