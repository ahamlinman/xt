use std::io::{self, Read, Write};
use std::str::FromStr;

use structopt::StructOpt;

fn main() {
  let opt = Opt::from_args();

  match opt.from {
    None | Some(Format::Json) | Some(Format::Yaml) => {
      let de = serde_yaml::Deserializer::from_reader(io::stdin());
      transcode_and_write(de, opt.to);
    }
    Some(Format::Toml) => {
      let mut s = String::new();
      io::stdin().lock().read_to_string(&mut s).unwrap();
      let mut de = toml::Deserializer::new(s.as_str());
      transcode_and_write(&mut de, opt.to);
    }
  }
}

fn transcode_and_write<'a, D>(de: D, to: Format)
where
  D: serde::de::Deserializer<'a>,
{
  match to {
    Format::Json => {
      let mut ser = serde_json::Serializer::pretty(io::stdout());
      serde_transcode::transcode(de, &mut ser).expect("failed to serialize JSON")
    }
    Format::Yaml => {
      let mut ser = serde_yaml::Serializer::new(io::stdout());
      serde_transcode::transcode(de, &mut ser).expect("failed to serialize YAML")
    }
    Format::Toml => {
      let mut s = String::new();
      let mut ser = toml::Serializer::new(&mut s);
      serde_transcode::transcode(de, &mut ser).expect("failed to serialize TOML");
      io::stdout().write(s.as_bytes()).unwrap();
    }
  };
}

#[derive(StructOpt, Debug)]
#[structopt(about)]
struct Opt {
  #[structopt(short = "t", help = "Format to convert to", default_value = "json")]
  to: Format,

  #[structopt(short = "f", help = "Format to convert from")]
  from: Option<Format>,
}

#[derive(Debug)]
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
