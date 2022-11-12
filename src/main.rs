#![deny(
	// Enforce some additional strictness on unsafe code.
	unsafe_op_in_unsafe_fn,
	clippy::undocumented_unsafe_blocks,
	// Deny a number of `as` casts in favor of safer alternatives.
	clippy::as_underscore,
	clippy::ptr_as_ptr,
	clippy::cast_lossless,
	clippy::cast_possible_truncation,
	clippy::checked_conversions,
	clippy::unnecessary_cast,
	// More general style-type things.
	clippy::from_over_into,
	clippy::semicolon_if_nothing_returned,
)]

use std::error;
use std::fmt;
use std::fs::File;
use std::io::{self, BufWriter};
use std::iter::FusedIterator;
use std::path::{Path, PathBuf};
use std::process;

use xt::Format;

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

fn main() {
	let args = match Cli::parse_args() {
		Ok(args) => args,
		Err(err) => xt_fail!("{}", err),
	};

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
		if let Err(err) = translator.flush() {
			xt_io_error!(&err);
		}
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
		next = err.source();
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
	// SAFETY: libc is assumed to work correctly. Everything in the block comes
	// from libc, so there are no Rust invariants to violate.
	unsafe {
		libc::signal(libc::SIGPIPE, libc::SIG_DFL);
		libc::raise(libc::SIGPIPE);
	}

	// Non-Unix systems fall back to a normal silent exit (and Unix systems
	// should not reach this line).
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
///            files.
///
///      yaml  Multi-document with "---" syntax. Default format for .yaml and
///            .yml files.
///
///      toml  Single documents only. Default format for .toml files.
///
///   msgpack  Multi-document as values are naturally self-delineating. Default
///            format for .msgpack files.
///
/// When xt does not know an input's format from a file extension or -f option,
/// it will attempt to detect the format using an unspecified algorithm that is
/// subject to change.
///
/// xt does not guarantee that every translation is possible, or lossless, or
/// reversible. xt's behavior is undefined if an input file is modified while
/// running.
struct Cli {
	input_paths: Vec<PathBuf>,
	to: Format,
	from: Option<Format>,
}

impl Cli {
	fn parse_args() -> Result<Self, lexopt::Error> {
		use lexopt::prelude::*;

		let mut input_paths: Vec<PathBuf> = vec![];
		let mut to: Option<Format> = None;
		let mut from: Option<Format> = None;

		let mut parser = lexopt::Parser::from_env();
		while let Some(arg) = parser.next()? {
			match arg {
				Short('f') => {
					from = Some(parser.value()?.parse_with(try_parse_format)?);
				}
				Short('t') => {
					to = Some(parser.value()?.parse_with(try_parse_format)?);
				}
				Value(val) => {
					input_paths.push(PathBuf::from(val));
				}
				Short('h') | Long("help") => {
					xt_fail!("help is not supported");
				}
				Short('V') | Long("version") => {
					xt_fail!("version is not supported");
				}
				_ => return Err(arg.unexpected()),
			}
		}

		Ok(Cli {
			input_paths,
			from,
			to: to.unwrap_or(Format::Json),
		})
	}
}

fn try_parse_format(s: &str) -> Result<Format, &'static str> {
	match s {
		"j" | "json" => Ok(Format::Json),
		"y" | "yaml" => Ok(Format::Yaml),
		"t" | "toml" => Ok(Format::Toml),
		"m" | "msgpack" => Ok(Format::Msgpack),
		_ => Err("not a valid format name"),
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
		// (UN)SAFETY: It is Undefined Behavior to modify a mapped file outside
		// of the process… so we tell users not to do that in the help output.
		// Sadly, this is NOT a real solution and does NOT provide any actual
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
