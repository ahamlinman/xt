use std::io::{self, Read, Write};
use std::str::FromStr;

use structopt::StructOpt;

fn main() {
  let opt = Opt::from_args();

  let input = io::stdin();
  let output = io::stdout();

  match opt.from {
    None | Some(Format::Yaml) => {
      let de = serde_yaml::Deserializer::from_reader(input);
      transcode_to(de, opt.to, output);
    }
    Some(Format::Json) => {
      let mut de = serde_json::Deserializer::from_reader(input);
      transcode_to(&mut de, opt.to, output);
    }
    Some(Format::Toml) => {
      let mut s = String::new();
      input.lock().read_to_string(&mut s).unwrap();
      let mut de = toml::Deserializer::new(s.as_str());
      transcode_to(&mut de, opt.to, output);
    }
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
