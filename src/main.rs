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
	init_sigpipe_handling();

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

	let mut output = BufWriter::new(io::stdout());
	if atty::is(atty::Stream::Stdout) && format_is_unsafe_for_terminal(args.to) {
		xt_fail!("refusing to output {} to a terminal", args.to);
	}

	let mut input = args
		.input_filename
		.as_ref()
		.map(Input::open)
		.unwrap_or(Ok(Input::Stdin))
		.unwrap_or_else(|err| xt_fail!(err));
	let handle = match &mut input {
		Input::Stdin => xt::Handle::from_reader(io::stdin()),
		Input::File(file) => xt::Handle::from_reader(file),
		Input::Mmap(map) => xt::Handle::from_slice(map),
	};
	let from = args.from.or_else(|| {
		args.input_filename
			.as_ref()
			.and_then(try_get_format_from_path)
	});

	let result = xt::translate(handle, from, args.to, &mut output);

	macro_rules! xt_fail_for_error {
		($x:expr) => {{
			if is_broken_pipe($x) {
				// Fail silently. This is a fallback for non-Unix platforms
				// where SIGPIPE won't kill us automatically.
				process::exit(1);
			}
			xt_fail!($x);
		}};
	}

	// Some serializers, including the one for YAML, don't expose broken pipe
	// errors in the error chain produced during transcoding. This check does a
	// decent job of catching those cases.
	if let Err(err) = output.flush() {
		xt_fail_for_error!(&err);
	}

	// Some other serializers, including the one for TOML, don't trigger the
	// above check but do expose broken pipe errors in their error chain (maybe
	// they flush internally?). So we still have to check this case in addition
	// to the above.
	if let Err(err) = result {
		xt_fail_for_error!(err.as_ref());
	}
}

fn init_sigpipe_handling() {
	// Restore the default behavior of SIGPIPE for Unix(-like) systems on
	// non-debug builds, so that broken pipes kill the process with a signal
	// rather than allow it to exit with a normal failure code.
	//
	// This is an admittedly imperfect attempt to satisfy two goals:
	//
	// 1. Match the behavior of other Unix utilities in this situation as
	//    closely as possible. Being killed by a signal is fundamentally
	//    different from exiting with any particular code, and xt should respect
	//    that rather than hack around it (e.g. it should not exit with code 141
	//    simply because that's what a shell would produce).
	//
	// 2. Ensure that the portable error handling paths for broken pipes on
	//    non-Unix systems receive at least some testing from developers on Unix
	//    systems. The logic to remain silent on broken pipe errors is
	//    surprisingly complex due to inconsistent I/O error reporting among
	//    xt's dependencies, so it would be bad for this logic to be too hard to
	//    exercise as changes are made.
	//
	// It's intentional that this is treated as a form of debug assertion. The
	// expectation is that a proper Unix system will reliably couple SIGPIPE to
	// EPIPE (human-sent signals don't count), and that xt is simple enough that
	// replacing a graceful return to main with an instant kill from deep in the
	// call stack doesn't change its observable behavior enough to matter (as
	// the only meaningful observation of behavior was through the now-broken
	// pipe). This is obviously not true for a wide range of other programs.
	//
	// TODO: Honestly, this does feel hard to justify compared to simply exiting
	// with code 1 on all platforms like ripgrep does. How reasonable is it that
	// something other than a shell would run xt, intentionally break its output
	// pipe, and then care whether it exited via a code or a signal?
	//
	// References:
	//
	// - https://github.com/rust-lang/rust/issues/62569
	// - https://stackoverflow.com/a/65760807
	// - https://github.com/BurntSushi/ripgrep/issues/200#issuecomment-616884727
	#[cfg(all(unix, not(debug_assertions)))]
	unsafe {
		libc::signal(libc::SIGPIPE, libc::SIG_DFL);
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
#[derive(Parser)]
#[clap(version, about = SHORT_HELP, verbatim_doc_comment)]
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
