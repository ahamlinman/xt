use std::error::Error;
use std::fs::File;
use std::io::{self, BufWriter, Write};
use std::path::PathBuf;
use std::process;

use clap::ErrorKind::{HelpDisplayed, VersionDisplayed};
use structopt::StructOpt;

use jyt::{jyt, Format, InputHandle};

fn main() {
  let opt = match Opt::from_args_safe() {
    Ok(opt) => opt,
    Err(err) => match err.kind {
      HelpDisplayed | VersionDisplayed => err.exit(),
      _ => {
        // As of this writing, clap's error messages (other than those above)
        // include an "error:" prefix, so this gives us consistent formatting
        // for both argument and translation errors. It is a bit fragile, since
        // it's unlikely that clap's error message format is guaranteed to be
        // stable.
        eprint!("jyt {}\n", err.message);
        process::exit(1);
      }
    },
  };

  macro_rules! jyt_exit {
    ($fmt:literal, $($x:expr),*) => {{
      eprint!(concat!("jyt error: ", $fmt, "\n"), $($x),*);
      process::exit(1);
    }};
    ($x:expr) => {
      jyt_exit!("{}", $x)
    };
  }

  let mut output = BufWriter::new(io::stdout());
  if atty::is(atty::Stream::Stdout) && !format_is_safe_for_terminal(opt.to) {
    jyt_exit!("refusing to output {} to a terminal", opt.to);
  }

  let input = match opt.input() {
    Ok(input) => input,
    Err(err) => jyt_exit!(err),
  };

  let jyt_err = jyt(input, opt.detect_from(), opt.to, &mut output);

  // Some of our serializers, particularly the YAML serializer, don't expose
  // underlying I/O errors in their error chain when they occur. This check
  // gives us a more direct indication of errors related to writing output,
  // particularly broken pipe errors which we'd prefer to hide.
  if let Err(err) = output.flush() {
    match is_broken_pipe(&err) {
      true => return,
      false => jyt_exit!(err),
    }
  }

  if let Err(err) = jyt_err {
    jyt_exit!(err);
  }
}

fn format_is_safe_for_terminal(format: Format) -> bool {
  match format {
    Format::Msgpack => false,
    _ => true,
  }
}

fn is_broken_pipe(err: &(dyn Error + 'static)) -> bool {
  use io::ErrorKind::BrokenPipe;

  let mut next = Some(err);
  while let Some(err) = next {
    match err.downcast_ref::<io::Error>() {
      Some(err) if err.kind() == BrokenPipe => return true,
      _ => {
        next = err.source();
      }
    }
  }

  false
}

#[derive(StructOpt)]
#[structopt(verbatim_doc_comment)]
/// Translate between serialized data formats
///
/// This version of jyt supports the following formats, each of which may be
/// specified by full name or first character (e.g. '-ty' == '-t yaml'):
///
///      json  Multi-document with self-delineating values (object, array,
///            string) or whitespace between values. Default format for .json
///            files. Supports streaming input.
///
///      yaml  Multi-document with "---" syntax. Default format for .yaml and
///            .yml files. UTF-8 encoding only.
///
///      toml  Single documents only. Default format for .toml files.
///
///   msgpack  Multi-document as values are naturally self-delineating. Default
///            format for .msgpack files. Supports streaming input.
///
/// Some multi-document input formats can translate a stream of documents
/// without buffering all input into memory first. The input format must be
/// known in advance to enable streaming, usually with an explicit -f.
///
/// When the input format is not known in advance with an explicit -f or file
/// extension, jyt will attempt to auto-detect it by buffering all input into
/// memory and running an unspecified algorithm that is subject to change.
///
/// jyt does not guarantee that every translation is possible, or lossless, or
/// reversible. jyt's behavior is undefined if an input file is modified while
/// running. jyt is not designed for use with untrusted input.
struct Opt {
  #[structopt(
    short = "t",
    help = "Format to convert to",
    default_value = "json",
    parse(try_from_str = try_parse_format),
  )]
  to: Format,

  #[structopt(
    short = "f",
    help = "Format to convert from",
    parse(try_from_str = try_parse_format),
  )]
  from: Option<Format>,

  #[structopt(
    name = "file",
    help = "File to read input from [default: stdin]",
    parse(from_os_str)
  )]
  input_filename: Option<PathBuf>,
}

fn try_parse_format(s: &str) -> Result<Format, String> {
  match s {
    "j" | "json" => Ok(Format::Json),
    "y" | "yaml" => Ok(Format::Yaml),
    "t" | "toml" => Ok(Format::Toml),
    "m" | "msgpack" => Ok(Format::Msgpack),
    _ => Err(format!("'{}' is not a valid format", s)),
  }
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
        Some("msgpack") => Some(Format::Msgpack),
        _ => None,
      },
    }
  }

  fn input(&self) -> io::Result<InputHandle> {
    match &self.input_filename {
      None => Ok(InputHandle::from_reader(io::stdin())),
      Some(path) if path.to_str() == Some("-") => Ok(InputHandle::from_reader(io::stdin())),
      Some(path) => {
        let file = File::open(&path)?;
        // Safety: Modification of the mapped file outside the process triggers
        // undefined behavior. Our dirty "solution" is to document this in the
        // help output.
        match unsafe { memmap2::MmapOptions::new().populate().map(&file) } {
          // Per memmap2 docs, it's safe to drop file once mmap succeeds.
          Ok(map) => Ok(InputHandle::from_buffer(map)),
          // Fall back to using a reader, in case the file is actually something
          // like a named pipe.
          Err(_) => Ok(InputHandle::from_reader(file)),
        }
      }
    }
  }
}
