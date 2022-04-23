use std::error::Error;
use std::fs::File;
use std::io::{self, BufWriter, Write};
use std::path::PathBuf;
use std::process;

use clap::{
  ErrorKind::{DisplayHelp, DisplayVersion},
  Parser,
};

use xt::{Format, InputHandle};

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
        eprint!("xt {}", err);
        process::exit(1);
      }
    },
  };

  macro_rules! xt_fail {
    ($fmt:literal, $($x:expr),*) => {{
      eprintln!(concat!("xt error: ", $fmt), $($x),*);
      process::exit(1);
    }};
    ($x:expr) => {
      xt_fail!("{}", $x)
    };
  }

  let mut output = BufWriter::new(io::stdout());
  if atty::is(atty::Stream::Stdout) && format_is_unsafe_for_terminal(args.to) {
    xt_fail!("refusing to output {} to a terminal", args.to);
  }

  let mut input = args.input().unwrap_or_else(|err| xt_fail!(err));
  let input_handle = match &mut input {
    Input::Stdin => InputHandle::from_reader(io::stdin()),
    Input::File(file) => InputHandle::from_reader(file),
    Input::Mmap(map) => InputHandle::from_slice(map),
  };

  let result = xt::translate(input_handle, args.detect_from(), args.to, &mut output);

  // Some serializers, including the one for YAML, don't expose broken pipe
  // errors in the error chain produced during transcoding. This check does a
  // decent job of catching those cases.
  if let Err(err) = output.flush() {
    match is_broken_pipe(&err) {
      true => return,
      false => xt_fail!(err),
    }
  }

  // Some other serializers, including the one for TOML, don't trigger the above
  // check but do expose broken pipe errors in their error chain (maybe they
  // flush internally?). So we still have to check this case in addition to the
  // above.
  if let Err(err) = result {
    if !is_broken_pipe(err.as_ref()) {
      xt_fail!(err)
    }
  }
}

fn format_is_unsafe_for_terminal(format: Format) -> bool {
  matches!(format, Format::Msgpack)
}

fn is_broken_pipe(err: &(dyn Error + 'static)) -> bool {
  use io::ErrorKind::BrokenPipe;
  let mut next = Some(err);
  while let Some(err) = next {
    if matches!(err.downcast_ref::<io::Error>(), Some(err) if err.kind() == BrokenPipe) {
      return true;
    }
    next = err.source()
  }
  false
}

const SHORT_HELP: &str = "Translate between serialized data formats

Use --help for full usage information and available formats.";

#[derive(Parser)]
#[clap(version, about = SHORT_HELP, verbatim_doc_comment)]
/// Translate between serialized data formats
///
/// This version of xt supports the following formats, each of which may be
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
/// Input formats that support streaming can translate individual documents in
/// an unbounded stream as they appear. Formats that do not support streaming
/// must load all input into memory before translating any of it.
///
/// When xt does not know the input format from a file extension or explicit -f,
/// it will attempt to detect the format using an unspecified algorithm that is
/// subject to change. If an unbounded stream does not match a format that
/// supports streaming, the detector will load the entire stream into memory.
///
/// xt does not guarantee that every translation is possible, or lossless, or
/// reversible. xt's behavior is undefined if an input file is modified while
/// running. xt is not designed for use with untrusted input.
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

enum Input {
  Stdin,
  File(std::fs::File),
  Mmap(memmap2::Mmap),
}

impl Cli {
  fn detect_from(&self) -> Option<Format> {
    if self.from.is_some() {
      return self.from;
    }
    match &self.input_filename {
      None => None,
      Some(path) => match path.extension().and_then(|ext| ext.to_str()) {
        Some("json") => Some(Format::Json),
        Some("yaml" | "yml") => Some(Format::Yaml),
        Some("toml") => Some(Format::Toml),
        Some("msgpack") => Some(Format::Msgpack),
        _ => None,
      },
    }
  }

  fn input(&self) -> io::Result<Input> {
    match &self.input_filename {
      None => Ok(Input::Stdin),
      Some(path) if path.to_str() == Some("-") => Ok(Input::Stdin),
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
          Ok(map) => Ok(Input::Mmap(map)),
          // If mmap fails, we can always fall back to reading the file
          // normally. Examples of where this can matter include (but are not
          // limited to) process substitution and named pipes.
          Err(_) => Ok(Input::File(file)),
        }
      }
    }
  }
}
