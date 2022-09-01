use std::error;
use std::fmt;
use std::fs::File;
use std::io::{self, BufWriter, Write};
use std::iter::FusedIterator;
use std::path::{Path, PathBuf};
use std::process;

use clap::{
	ErrorKind::{DisplayHelp, DisplayVersion},
	Parser,
};

use xt::Format;

fn main() {
	let args = match Cli::try_parse() {
		Ok(args) => args,
		Err(err) => match err.kind() {
			DisplayHelp | DisplayVersion => err.exit(),
			_ => {
				// As of this writing, clap's error messages (other than those
				// above) include an "error:" prefix, so this gives us
				// consistent formatting for both argument and translation
				// errors. It is a bit fragile, since it's unlikely that clap's
				// error message format is guaranteed to be stable.
				eprint!("xt {}", err);
				process::exit(1);
			}
		},
	};

	macro_rules! xt_fail {
		($path:ident, $fmt:literal $(, $($x:expr),+)?) => {{
			eprintln!(concat!("xt error in {}: ", $fmt), $path $(, $($x),+)?);
			process::exit(1);
		}};
		($fmt:literal, $($x:expr),*) => {{
			eprintln!(concat!("xt error: ", $fmt), $($x),*);
			process::exit(1);
		}};
		($path:ident, $x:expr) => { xt_fail!($path, "{}", $x) };
		($x:expr) => { xt_fail!("{}", $x) };
	}

	macro_rules! xt_io_error {
		(@check_pipe $x:expr) => {
			if is_broken_pipe($x) {
				exit_for_broken_pipe();
			}
		};
		($path:ident, $x:expr) => {{
			xt_io_error!(@check_pipe $x);
			xt_fail!($path, "{}", $x);
		}};
		($x:expr) => {{
			xt_io_error!(@check_pipe $x);
			xt_fail!("{}", $x);
		}};
	}

	if atty::is(atty::Stream::Stdout) && format_is_unsafe_for_terminal(args.to) {
		xt_fail!("refusing to output {} to a terminal", args.to);
	}

	let mut stdin_used = false;
	let mut output = BufWriter::new(io::stdout());
	let mut translator = xt::Translator::new(&mut output, args.to);

	let input_paths = if args.input_paths.is_empty() {
		InputPathIter::stdin_only()
	} else {
		InputPathIter::paths(args.input_paths.into_iter().map(Into::into))
	};

	for path in input_paths {
		let mut input = path.open().unwrap_or_else(|err| xt_fail!(path, err));
		if let Input::Stdin = input {
			if stdin_used {
				xt_fail!("cannot read from stdin more than once");
			} else {
				stdin_used = true;
			}
		}

		let from = args.from.or_else(|| path.format());
		let handle = match &mut input {
			Input::Stdin => xt::Handle::from_reader(io::stdin()),
			Input::File(file) => xt::Handle::from_reader(file),
			Input::Mmap(map) => xt::Handle::from_slice(map),
		};

		if let Err(err) = translator.translate(handle, from) {
			xt_io_error!(path, err.as_ref());
		}
	}

	if let Err(err) = output.flush() {
		xt_io_error!(&err);
	}
}

fn format_is_unsafe_for_terminal(format: Format) -> bool {
	matches!(format, Format::Msgpack)
}

fn is_broken_pipe(err: &(dyn error::Error + 'static)) -> bool {
	let mut next = Some(err);
	while let Some(err) = next {
		if let Some(ioerr) = err.downcast_ref::<io::Error>() {
			if ioerr.kind() == io::ErrorKind::BrokenPipe {
				return true;
			}
		}
		next = err.source()
	}
	false
}

fn exit_for_broken_pipe() {
	// For largely historical reasons, Rust ignores SIGPIPE on Unix(-like)
	// systems by default and treats writing to a broken pipe exclusively as a
	// normal I/O error. Dying from SIGPIPE in this case more closely matches
	// the behavior of a typical Unix command line program. However, we still
	// come here through the regular error handling path instead of restoring
	// the default SIGPIPE handler from the start, to ensure that developers on
	// Unix don't accidentally write non-portable error handling logic.
	//
	// https://github.com/rust-lang/rust/issues/62569
	// https://stackoverflow.com/a/65760807
	// https://github.com/BurntSushi/ripgrep/issues/200#issuecomment-616884727
	#[cfg(unix)]
	unsafe {
		libc::signal(libc::SIGPIPE, libc::SIG_DFL);
		libc::raise(libc::SIGPIPE);
	}

	// For non-Unix systems, we just fall back to a plain silent exit.
	#[cfg(not(unix))]
	process::exit(1);
}

const SHORT_HELP: &str = "Translate between serialized data formats

Use --help for full usage information and available formats.";

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
/// When xt does not know an input's format from a file extension or -f option,
/// it will attempt to detect the format using an unspecified algorithm that is
/// subject to change. If an unbounded stream does not match a format that
/// supports streaming, the detector will load the entire stream into memory.
///
/// xt does not guarantee that every translation is possible, or lossless, or
/// reversible. xt's behavior is undefined if an input file is modified while
/// running. xt is not designed for use with untrusted input.
#[derive(Parser)]
#[clap(version, about = SHORT_HELP, verbatim_doc_comment)]
struct Cli {
	#[clap(
		name = "files",
		help = "Files to translate [default: stdin]",
		parse(from_os_str)
	)]
	input_paths: Vec<PathBuf>,

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

#[derive(PartialEq, Eq)]
enum InputPath {
	Stdin,
	File(PathBuf),
}

enum Input {
	Stdin,
	File(std::fs::File),
	Mmap(memmap2::Mmap),
}

impl From<PathBuf> for InputPath {
	fn from(path: PathBuf) -> Self {
		if path == Path::new("-") {
			Self::Stdin
		} else {
			Self::File(path)
		}
	}
}

impl InputPath {
	fn open(&self) -> io::Result<Input> {
		let path = match self {
			Self::Stdin => return Ok(Input::Stdin),
			Self::File(path) => path,
		};
		let file = File::open(path)?;
		// "SAFETY": It is undefined behavior to modify a mapped file outside of
		// the process... so we tell users not to do that in the help output.
		// No, this is not a real solution and does not provide any actual
		// safety guarantee. It's a risk we take intentionally in the name of
		// performance, based on a pragmatic understanding of the failure modes
		// most likely to appear when the requirement is violated.
		match unsafe { memmap2::MmapOptions::new().populate().map(&file) } {
			// Per memmap2 docs, it's safe to drop the file once mmap succeeds.
			Ok(map) => Ok(Input::Mmap(map)),
			// If mmap fails, fall back to reading the file normally. Examples
			// of where this can matter include (but are not limited to) process
			// substitution and named pipes.
			Err(_) => Ok(Input::File(file)),
		}
	}

	fn format(&self) -> Option<Format> {
		let path = match self {
			Self::Stdin => return None,
			Self::File(path) => path,
		};
		match path
			.extension()
			.and_then(|ext| ext.to_str())
			.map(|ext| ext.to_ascii_lowercase())
			.as_deref()
		{
			Some("json") => Some(Format::Json),
			Some("yaml" | "yml") => Some(Format::Yaml),
			Some("toml") => Some(Format::Toml),
			Some("msgpack") => Some(Format::Msgpack),
			_ => None,
		}
	}
}

impl fmt::Display for InputPath {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Stdin => f.write_str("standard input"),
			Self::File(path) => path.display().fmt(f),
		}
	}
}

enum InputPathIter<I>
where
	I: Iterator<Item = InputPath>,
{
	StdinOnly { used: bool },
	Paths(I),
}

impl<I> InputPathIter<I>
where
	I: Iterator<Item = InputPath>,
{
	fn stdin_only() -> Self {
		Self::StdinOnly { used: false }
	}

	fn paths(iter: I) -> Self {
		Self::Paths(iter)
	}
}

impl<I> Iterator for InputPathIter<I>
where
	I: Iterator<Item = InputPath>,
{
	type Item = InputPath;

	fn next(&mut self) -> Option<Self::Item> {
		match self {
			Self::StdinOnly { used: true } => None,
			Self::StdinOnly { used: false } => {
				*self = Self::StdinOnly { used: true };
				Some(InputPath::Stdin)
			}
			Self::Paths(iter) => iter.next(),
		}
	}
}

impl<I> FusedIterator for InputPathIter<I> where I: Iterator<Item = InputPath> + FusedIterator {}
