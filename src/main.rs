use std::error::Error;
use std::fs::File;
use std::io::{self, BufReader, BufWriter, Read, Write};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::process;
use std::str::{self, FromStr};

use memmap2::Mmap;
use serde::Deserialize;
use structopt::StructOpt;

fn main() {
  let opt = match Opt::from_args_safe() {
    Ok(opt) => opt,
    Err(e) => match e.kind {
      clap::ErrorKind::HelpDisplayed | clap::ErrorKind::VersionDisplayed => e.exit(),
      _ => {
        // As of this writing, clap's "real" error messages include an "error:"
        // prefix, so this gives consistent formatting for both argument and
        // translation errors. It is admittedly a bit fragile, since I can't
        // imagine clap's error message format having any stability guarantee.
        eprint!("jyt {}\n", e.message);
        process::exit(1);
      }
    },
  };

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
      // serde_yaml has a "from_reader" method, however as of this writing it
      // "secretly" buffers all the reader's contents into a byte vector, so we
      // may as well give it our own slice.
      let slice = get_input_slice(opt.input_file)?;
      let de = serde_yaml::Deserializer::from_slice(&slice);
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

fn get_input_reader<P>(path: Option<P>) -> io::Result<impl Read>
where
  P: AsRef<Path>,
{
  let reader: Box<dyn Read> = match filter_stdin_path(path) {
    None => Box::new(io::stdin()),
    Some(p) => Box::new(File::open(p)?),
  };
  Ok(BufReader::new(reader))
}

fn get_input_slice<P>(path: Option<P>) -> io::Result<Box<dyn Deref<Target = [u8]>>>
where
  P: AsRef<Path>,
{
  let mut input: Box<dyn Read> = match filter_stdin_path(path) {
    None => Box::new(io::stdin()),
    Some(p) => {
      // mmap the file to represent it directly as a slice, or fall back to
      // standard buffering if that fails.
      //
      // Mmap::map is marked unsafe as modifying a mapped file outside of the
      // process can produce undefined behavior. Our dirty "solution" is simply
      // to document this for users.
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

fn transcode_to<'de, D>(de: D, to: Format) -> Result<(), Box<dyn Error>>
where
  D: serde::de::Deserializer<'de>,
{
  // Note that BufWriter attempts to flush when dropped, but ignores flush
  // errors. This is fine, we only drop before flushing if a transcode error
  // forces us to abort early, in which case the real error happened during
  // transcoding.
  let mut output = BufWriter::new(io::stdout());

  match to {
    Format::Json if atty::is(atty::Stream::Stdout) => {
      let mut ser = serde_json::Serializer::pretty(&mut output);
      serde_transcode::transcode(de, &mut ser)?;
      println!(""); // Extra newline so it looks even prettier
    }
    Format::Json => {
      let mut ser = serde_json::Serializer::new(&mut output);
      serde_transcode::transcode(de, &mut ser)?;
    }
    Format::Yaml => {
      let mut ser = serde_yaml::Serializer::new(&mut output);
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
      output.write_all(output_buf.as_bytes())?;
    }
  }

  output.flush()?;
  Ok(())
}

#[derive(StructOpt)]
#[structopt(verbatim_doc_comment)]
/// Translate between serialized data formats
///
/// Supported formats are: json, yaml, and toml. Formats can be specified using
/// the first character of the name, e.g. '-ty' is the same as '-t yaml'.
///
/// jyt will try to detect an input format for files based on their extension.
/// Otherwise it defaults to '-f yaml', which supports YAML and JSON input (but
/// is slightly less efficient than '-f json' for the latter). To read TOML from
/// stdin or a file with a non-standard extension, specify '-f toml' (a.k.a.
/// '-ft') explicitly.
///
/// When reading files, jyt may attempt to map the file into memory. jyt's
/// behavior is undefined if a mapped file is modified while jyt is running.
///
/// With JSON and YAML output, jyt will output values in the same order as the
/// input. With TOML output, jyt will parse all input into memory, then sort
/// non-table values before table values in the output to ensure it is valid
/// TOML. TOML does not support null values; translation will fail if the input
/// contains one.
///
/// With JSON output, jyt pretty-prints if outputting to a terminal, and prints
/// compactly otherwise. jyt outputs YAML and TOML with consistent formatting to
/// all destinations.
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
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "j" | "json" => Ok(Self::Json),
      "y" | "yaml" => Ok(Self::Yaml),
      "t" | "toml" => Ok(Self::Toml),
      _ => Err(format!("unknown format '{}'", s)),
    }
  }
}
