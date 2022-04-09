use std::error::Error;
use std::fs::File;
use std::io::{self, BufWriter, Write};
use std::path::PathBuf;
use std::process;

use clap::{
  ErrorKind::{DisplayHelp, DisplayVersion},
  Parser,
};

use jyt::{jyt, Format, InputHandle};

fn main() {
  let args = match Cli::try_parse() {
    Ok(args) => args,
    Err(err) => match err.kind() {
      DisplayHelp | DisplayVersion => err.exit(),
      _ => {
        // As of this writing, clap's error messages (other than those above)
        // include an "error:" prefix, so this gives us consistent formatting
        // for both argument and translation errors. It is a bit fragile, since
        // it's unlikely that clap's error message format is guaranteed to be
        // stable.
        eprint!("jyt {}", err);
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
  if atty::is(atty::Stream::Stdout) && !format_is_safe_for_terminal(args.to) {
    jyt_exit!("refusing to output {} to a terminal", args.to);
  }

  let input = match args.input() {
    Ok(input) => input,
    Err(err) => jyt_exit!(err),
  };

  let jyt_err = jyt(input, args.detect_from(), args.to, &mut output);

  // Some serializers, including the one for YAML, don't expose broken pipe
  // errors in the error chain produced during transcoding. This check does a
  // decent job of catching those cases.
  if let Err(err) = output.flush() {
    match is_broken_pipe(&err) {
      true => return,
      false => jyt_exit!(err),
    }
  }

  // Some other serializers, including the one for TOML, don't trigger the above
  // check but do expose broken pipe errors in their error chain (maybe they
  // flush internally?). So we still have to check this case in addition to the
  // above.
  if let Err(err) = jyt_err {
    match is_broken_pipe(err.as_ref()) {
      true => return,
      false => jyt_exit!(err),
    }
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

const SHORT_HELP: &'static str = "Translate between serialized data formats

Use --help for full usage information and available formats.";

#[derive(Parser)]
#[clap(version, about = SHORT_HELP, verbatim_doc_comment)]
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
///            .yml files.
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
struct Cli {
  #[clap(
    name = "file",
    help = "File to read input from [default: stdin]",
    parse(from_os_str)
  )]
  input_filename: Option<PathBuf>,

  #[clap(
    short = 't',
    help = "Format to convert to",
    help_heading = "OPTIONS",
    default_value = "json",
    parse(try_from_str = try_parse_format),
  )]
  to: Format,

  #[clap(
    short = 'f',
    help = "Format to convert from",
    help_heading = "OPTIONS",
    parse(try_from_str = try_parse_format),
  )]
  from: Option<Format>,

  #[clap(
    short = 'h',
    long,
    help = "Prints help information",
    help_heading = "FLAGS"
  )]
  help: bool,

  #[clap(
    short = 'V',
    long,
    help = "Prints version information",
    help_heading = "FLAGS"
  )]
  version: bool,
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

impl Cli {
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
        let file = File::open(path)?;
        // "SAFETY": It is undefined behavior to modify a mapped file outside of
        // the process... so we tell users not to do that in the help output.
        // No, this is not a real solution and does not provide any actual
        // safety guarantee. It's a risk intentionally taken in the name of
        // performance, based on a pragmatic understanding of the failure modes
        // most likely to appear when the requirement is violated.
        match unsafe { memmap2::MmapOptions::new().populate().map(&file) } {
          // Per memmap2 docs, it's safe to drop file once mmap succeeds.
          Ok(map) => Ok(InputHandle::from_buffer(map)),
          // If mmap fails, we can always fall back to reading the file
          // normally. Examples of where this can matter include (but are not
          // limited to) process substitution and named pipes.
          Err(_) => Ok(InputHandle::from_reader(file)),
        }
      }
    }
  }
}
