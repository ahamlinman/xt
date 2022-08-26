use std::error::Error;
use std::fs::File;
use std::io::{self, BufWriter, Write};
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
		($fmt:literal, $($x:expr),*) => {{
			eprintln!(concat!("xt error: ", $fmt), $($x),*);
			process::exit(1);
		}};
		($x:expr) => {
			xt_fail!("{}", $x)
		};
	}

	macro_rules! xt_fail_for_error {
		($x:expr) => {{
			if is_broken_pipe($x) {
				exit_for_broken_pipe();
			}
			xt_fail!($x);
		}};
	}

	if atty::is(atty::Stream::Stdout) && format_is_unsafe_for_terminal(args.to) {
		xt_fail!("refusing to output {} to a terminal", args.to);
	}

	let mut stdin_used = false;
	let mut output = BufWriter::new(io::stdout());
	let mut translator = xt::Translator::new(&mut output, args.to);

	let input_paths = if !args.input_paths.is_empty() {
		args.input_paths
	} else {
		vec![PathBuf::from("-")]
	};
	for path in input_paths {
		let mut input = Input::open(&path).unwrap_or_else(|err| xt_fail!(err));
		if let Input::Stdin = input {
			if stdin_used {
				xt_fail!("cannot read from stdin more than once");
			} else {
				stdin_used = true;
			}
		}

		let handle = match &mut input {
			Input::Stdin => xt::Handle::from_reader(io::stdin()),
			Input::File(file) => xt::Handle::from_reader(file),
			Input::Mmap(map) => xt::Handle::from_slice(map),
		};
		let from = args.from.or_else(|| try_get_format_from_path(&path));
		if let Err(err) = translator.translate(handle, from) {
			xt_fail_for_error!(err.as_ref());
		}
	}

	if let Err(err) = output.flush() {
		xt_fail_for_error!(&err);
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

fn exit_for_broken_pipe() {
	// For largely historical reasons, Rust ignores SIGPIPE on Unix(-like)
	// systems by default and treats writing to a broken pipe exclusively as a
	// normal I/O error. Dying from SIGPIPE in this case more closely matches
	// the behavior of a typical Unix command line program. However, we still
	// come here through the regular (portable) error handling path instead of
	// restoring the default SIGPIPE handler from the start, to ensure that
	// developers on Unix don't accidentally give non-Unix users a hard time.
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

fn try_get_format_from_path<P>(path: P) -> Option<Format>
where
	P: AsRef<Path>,
{
	match path
		.as_ref()
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

enum Input {
	Stdin,
	File(std::fs::File),
	Mmap(memmap2::Mmap),
}

impl Input {
	fn open<P>(path: P) -> io::Result<Input>
	where
		P: AsRef<Path>,
	{
		if path.as_ref().to_str() == Some("-") {
			return Ok(Input::Stdin);
		}

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
			// If mmap fails, we can always fall back to reading the file
			// normally. Examples of where this can matter include (but are not
			// limited to) process substitution and named pipes.
			Err(_) => Ok(Input::File(file)),
		}
	}
}
