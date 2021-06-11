use std::error::Error;
use std::fmt::{self, Display};
use std::fs::File;
use std::io::{self, BufReader, BufWriter, Read, Write};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::process;
use std::str::{self, FromStr};

use memmap2::MmapOptions;
use structopt::StructOpt;

fn main() {
  let opt = match Opt::from_args_safe() {
    Ok(opt) => opt,
    Err(e) => match e.kind {
      clap::ErrorKind::HelpDisplayed | clap::ErrorKind::VersionDisplayed => e.exit(),
      _ => {
        // As of this writing, clap's error messages (other than those above)
        // include an "error:" prefix, so this gives consistent formatting for
        // both argument and translation errors. It is a bit fragile, since it's
        // unlikely that clap's error message format is guaranteed to be stable.
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
  // Note that BufWriter attempts to flush when dropped, but ignores flush
  // errors. This is fine, we only drop before flushing if a transcode error
  // forces us to abort early, in which case the real error happened during
  // transcoding.
  let mut w = BufWriter::new(io::stdout());
  let pretty = atty::is(atty::Stream::Stdout);

  match opt.detect_from() {
    None | Some(Format::Yaml) => {
      // serde_yaml has a "from_reader" method, however as of this writing it
      // buffers all the reader's contents into a byte vector, so we may as well
      // give it a slice directly.
      let slice = get_input_slice(opt.input_file)?;
      for de in serde_yaml::Deserializer::from_slice(&slice) {
        transcode_to(de, &opt.to, &mut w, pretty)?;
      }
    }
    Some(Format::Json) => {
      let reader = get_input_reader(opt.input_file)?;
      let mut de = serde_json::Deserializer::from_reader(reader);
      transcode_to(&mut de, &opt.to, &mut w, pretty)?;
    }
    Some(Format::Toml) => {
      let slice = get_input_slice(opt.input_file)?;
      let input_str = str::from_utf8(&slice)?;
      let mut de = toml::Deserializer::new(input_str);
      transcode_to(&mut de, &opt.to, &mut w, pretty)?;
    }
  }

  w.flush()?;
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
      // This is marked unsafe as modifying a mapped file outside of the process
      // can produce undefined behavior. Our dirty "solution" is to document
      // this for users.
      let file = File::open(p)?;
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

fn filter_stdin_path<P>(path: Option<P>) -> Option<P>
where
  P: AsRef<Path>,
{
  path.filter(|p| p.as_ref().to_str() != Some("-"))
}

fn transcode_to<'de, D, W>(de: D, to: &Format, mut w: W, pretty: bool) -> Result<(), Box<dyn Error>>
where
  D: serde::de::Deserializer<'de>,
  W: Write,
{
  match to {
    Format::Json if pretty => {
      let mut ser = serde_json::Serializer::pretty(&mut w);
      serde_transcode::transcode(de, &mut ser)?;
      writeln!(&mut w, "")?;
    }
    Format::Json => {
      let mut ser = serde_json::Serializer::new(&mut w);
      serde_transcode::transcode(de, &mut ser)?;
      writeln!(&mut w, "")?;
    }
    Format::Yaml => {
      let mut ser = serde_yaml::Serializer::new(&mut w);
      serde_transcode::transcode(de, &mut ser)?;
    }
    _ => {
      panic!("attempted output to unsupported format {}", to)
    }
  }
  Ok(())
}

#[derive(StructOpt)]
#[structopt(verbatim_doc_comment)]
/// Translate between serialized data formats
///
/// This version of jyt supports the following formats, which may be specified
/// by their full name or first character (e.g. '-ty' == '-t yaml'):
///
///   json: Input and output
///   yaml: Input and output
///   toml: Input only
///
/// With file inputs, jyt will try to detect the input format based on file
/// extensions. Otherwise it defaults to '-f yaml', which supports YAML and JSON
/// input (but is less efficient than '-f json' for the latter). jyt's behavior
/// is undefined if an input file is modified while jyt is running.
///
/// Where a distinction is possible, jyt will print "pretty" output to
/// terminals, and "compact" output to other destinations.
struct Opt {
  #[structopt(
    short = "t",
    help = "Format to convert to",
    default_value = "json",
    parse(try_from_str = Opt::parse_to)
  )]
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
  fn parse_to(s: &str) -> Result<Format, <Format as FromStr>::Err> {
    let f = Format::from_str(s)?;
    if f.can_output() {
      Ok(f)
    } else {
      Err(format!("{} output is not supported", f))
    }
  }

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

impl Format {
  fn can_output(&self) -> bool {
    match self {
      Self::Json | Self::Yaml => true,
      Self::Toml => false,
    }
  }
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

impl Display for Format {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self::Json => write!(f, "json"),
      Self::Yaml => write!(f, "yaml"),
      Self::Toml => write!(f, "toml"),
    }
  }
}
