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
	clippy::needless_raw_string_hashes,
	clippy::semicolon_if_nothing_returned,
)]
#![warn(
	// Print macros can panic, and should only be for temporary debugging.
	clippy::print_stderr,
	clippy::print_stdout,
	// The following macros represent incomplete implementation work.
	clippy::todo,
	clippy::unimplemented,
	// Style-type things that might not need an _immediate_ fix.
	clippy::doc_markdown,
	clippy::similar_names,
)]

use std::borrow::Cow;
use std::env;
use std::fmt;
use std::fs::File;
use std::io::{self, BufWriter, IsTerminal, Write};
use std::path::{Path, PathBuf};
use std::process;

use xt::Format;

#[macro_use]
mod bail;
mod pipecheck;

fn main() {
	let args = match Cli::parse_args() {
		Ok(args) => args,
		Err(err) => {
			let mut stderr = io::stderr().lock();
			let _ = writeln!(stderr, "xt error: {err}");
			write_short_help(stderr);
			process::exit(2);
		}
	};

	let stdout = io::stdout();
	if stdout.is_terminal() && format_is_unsafe_for_terminal(args.to) {
		xt_bail!(
			"refusing to output {format} to a terminal",
			format = args.to,
		);
	}

	let mut stdin_used = false;
	let mut output = pipecheck::Writer::new(BufWriter::new(stdout.lock()));
	let mut translator = xt::Translator::new(&mut output, args.to);

	let input_paths = if args.input_pathnames.is_empty() {
		InputPaths::one(InputPath::Stdin)
	} else {
		InputPaths::many(args.input_pathnames.into_iter().map(Into::into))
	};
	for path in input_paths {
		let input = match path.open() {
			Ok(input) => input,
			Err(err) => xt_bail_path!(path, "{err}"),
		};
		if let Input::Stdin = input {
			// TODO: Is this check really worth it? We don't stop anyone from
			// passing /dev/stdin more than once.
			if stdin_used {
				xt_bail!("cannot read from standard input more than once");
			}
			stdin_used = true;
		}

		let from = args.from.or_else(|| path.extension_format());
		let result = match input {
			Input::Stdin => translator.translate_reader(io::stdin().lock(), from),
			Input::File(file) => translator.translate_reader(file, from),
			Input::Mmap(map) => translator.translate_slice(&map, from),
		};
		if let Err(err) = result {
			xt_bail_path!(path, "{err}");
		}
		if let Err(err) = translator.flush() {
			xt_bail!("{err}");
		}
	}
}

fn format_is_unsafe_for_terminal(format: Format) -> bool {
	matches!(format, Format::Msgpack)
}

struct Cli {
	input_pathnames: Vec<PathBuf>,
	from: Option<Format>,
	to: Format,
}

impl Cli {
	fn parse_args() -> Result<Self, lexopt::Error> {
		use lexopt::prelude::*;

		let mut input_pathnames: Vec<PathBuf> = vec![];
		let mut from: Option<Format> = None;
		let mut to: Option<Format> = None;

		let mut parser = lexopt::Parser::from_env();
		while let Some(arg) = parser.next()? {
			match arg {
				Short('f') => {
					if from.is_some() {
						return Err("cannot provide '-f' more than once".into());
					}
					from = Some(parser.value()?.parse_with(try_parse_format)?);
				}
				Short('t') => {
					if to.is_some() {
						return Err("cannot provide '-t' more than once".into());
					}
					to = Some(parser.value()?.parse_with(try_parse_format)?);
				}
				Value(val) => {
					input_pathnames.push(PathBuf::from(val));
				}
				Short('V') | Long("version") => {
					const VERSION: &str = version_string();
					let _ = writeln!(io::stdout().lock(), "{VERSION}");
					process::exit(0);
				}
				Short('h') => {
					write_short_help(io::stdout().lock());
					process::exit(0);
				}
				Long("help") => {
					print_long_help();
					process::exit(0);
				}
				_ => return Err(arg.unexpected()),
			}
		}

		Ok(Cli {
			input_pathnames,
			from,
			to: to.unwrap_or(Format::Json),
		})
	}
}

fn try_parse_format(s: &str) -> Result<Format, &'static str> {
	match s {
		"j" | "json" => Ok(Format::Json),
		"m" | "msgpack" => Ok(Format::Msgpack),
		"t" | "toml" => Ok(Format::Toml),
		"y" | "yaml" => Ok(Format::Yaml),
		_ => Err("not a valid format name"),
	}
}

/// A usage summary string shared across short and long help output.
static USAGE: &str = "[-f format] [-t format] [file ...]";

/// Writes short help output to the provided writer, ignoring errors.
fn write_short_help<W>(mut w: W)
where
	W: Write,
{
	let argv0 = usage_name();
	let _ = write!(
		w,
		r"Usage: {argv0} {USAGE}
Formats: json, msgpack, toml, yaml
Try '{argv0} --help' for more information.
"
	);
}

/// Writes long help output to standard output, ignoring errors.
fn print_long_help() {
	const VERSION: &str = version_string();
	let argv0 = usage_name();
	let _ = write!(
		io::stdout().lock(),
		r#"{VERSION} - Translate between serialized data formats

USAGE
    {argv0} {USAGE}

    Without -f, xt detects the format of each input by extension
    or content inspection.

    With no file, or with the special name "-" at any one position,
    xt translates from standard input.

OPTIONS
    -f format      Skip detection and convert every input from the given format
    -h, --help     Print a usage summary, then exit
    -t format      Convert to the given format (default: json)
    -V, --version  Print version information, then exit

FORMATS
    json, j
        Default for .json files.
        Multi-document (self-delineating or whitespace between values).

    msgpack, m
        Default for .msgpack files.
        Multi-document (naturally self-delineating).

    toml, t
        Default for .toml files.
        Single document per input or output.

    yaml, y
        Default for .yaml and .yml files.
        Multi-document (with --- or ... syntax).

CAVEATS
    xt does not guarantee that every translation is possible, or lossless, or
    reversible. xt's behavior is undefined if an input file is modified while
    running.
"#
	);
}

/// Returns the name of this program as it was invoked, or a default.
fn usage_name() -> Cow<'static, str> {
	if let Some(Ok(name)) = env::args_os().next().map(|s| s.into_string()) {
		Cow::Owned(name)
	} else {
		let name = env!("CARGO_PKG_NAME");
		Cow::Borrowed(if name.is_empty() { "xt" } else { name })
	}
}

/// Returns the full version string for the program (including the crate name)
/// based on Cargo metadata, or a default if Cargo metadata is unavailable.
const fn version_string() -> &'static str {
	if env!("CARGO_PKG_NAME").is_empty() || env!("CARGO_PKG_VERSION").is_empty() {
		"xt 0.0.0-unknown"
	} else {
		concat!(env!("CARGO_PKG_NAME"), " ", env!("CARGO_PKG_VERSION"))
	}
}

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
		let file = match self {
			Self::Stdin => return Ok(Input::Stdin),
			Self::File(path) => File::open(path)?,
		};

		// (UN)SAFETY: An Mmap provides access to arbitrary file data as a
		// &[u8], and it is Undefined Behavior for the data behind a &[u8] to
		// change. However, this CAN happen if the mapped file is modified
		// outside of the process.
		//
		// Our solution? We tell users not to do that in the help output.
		//
		// No, this is NOT a real solution, and does NOT provide any actual
		// safety guarantee. It's a risk taken intentionally based on a
		// pragmatic understanding of the failure modes most likely to appear
		// when the UB is invoked, i.e. that we are significantly more likely to
		// see xt die from a SIGBUS or silently produce invalid output than we
		// are to see demons *actually* fly out of somebody's nose (see
		// "Undefined behavior" on Wikipedia for more about this phenomenon).
		//
		// We take this risk because performance testing shows that translating
		// a slice is often significantly more efficient than translating a
		// reader, and because a memory map should give the OS smarter memory
		// management options compared to simply reading the contents of a file
		// into a buffer, especially when memory is at a premium or the file is
		// unusually large. However, this justification should be treated with
		// skepticism, and more research into possible safety enhancements is
		// likely warranted.
		if let Ok(map) = unsafe { memmap2::Mmap::map(&file) } {
			#[cfg(unix)]
			{
				// Make a best-effort attempt to advise the system that we will
				// soon access these pages sequentially. madvise failures on
				// these two values should not affect the normal functionality
				// of the mapping, so we intentionally ignore these Results.
				let _ = map.advise(memmap2::Advice::Sequential);
				let _ = map.advise(memmap2::Advice::WillNeed);
			}
			// Per memmap2 docs, it's safe to drop the original file now.
			return Ok(Input::Mmap(map));
		}

		// If mmap fails (process substitution, named pipes, etc.), fall back to
		// reader-based input.
		Ok(Input::File(file))
	}

	fn extension_format(&self) -> Option<Format> {
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
			Some("msgpack") => Some(Format::Msgpack),
			Some("toml") => Some(Format::Toml),
			Some("yaml" | "yml") => Some(Format::Yaml),
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

enum InputPaths<I>
where
	I: Iterator<Item = InputPath>,
{
	One(Option<InputPath>),
	Many(I),
}

impl<I> InputPaths<I>
where
	I: Iterator<Item = InputPath>,
{
	fn one(path: InputPath) -> Self {
		Self::One(Some(path))
	}

	fn many(iter: I) -> Self {
		Self::Many(iter)
	}
}

impl<I> Iterator for InputPaths<I>
where
	I: Iterator<Item = InputPath>,
{
	type Item = InputPath;

	fn next(&mut self) -> Option<Self::Item> {
		match self {
			Self::One(path) => path.take(),
			Self::Many(iter) => iter.next(),
		}
	}
}
