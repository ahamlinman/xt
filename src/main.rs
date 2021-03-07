use std::io;
use std::str::FromStr;

use structopt::StructOpt;

fn main() {
  let opt = Opt::from_args();

  let de = serde_yaml::Deserializer::from_reader(io::stdin());
  match opt.to {
    Format::Json => {
      let mut ser = serde_json::Serializer::pretty(io::stdout());
      serde_transcode::transcode(de, &mut ser).expect("failed to serialize JSON")
    }
    Format::Yaml => {
      let mut ser = serde_yaml::Serializer::new(io::stdout());
      serde_transcode::transcode(de, &mut ser).expect("failed to serialize YAML")
    }
  };
}

#[derive(StructOpt, Debug)]
#[structopt(about)]
struct Opt {
  #[structopt(short = "t", help = "Format to convert to", default_value = "json")]
  to: Format,
}

#[derive(Debug)]
enum Format {
  Json,
  Yaml,
}

impl FromStr for Format {
  type Err = &'static str;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "json" => Ok(Self::Json),
      "yaml" => Ok(Self::Yaml),
      _ => Err("unknown format"),
    }
  }
}
