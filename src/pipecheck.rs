//! Cross-platform Unix-style handling of broken pipe errors.

use std::io::{self, Write};

/// A writer that silently terminates the program on broken pipe errors.
///
/// When any call to its underlying writer returns a [`BrokenPipe`](io::ErrorKind::BrokenPipe)
/// error, a `Writer` will immediately terminate the program without returning
/// from the call. On Unix(-like) systems, the program will be terminated by a
/// SIGPIPE signal. Otherwise, it will exit with code 1.
///
/// # Why is this useful?
///
/// A Unix program that writes to a broken pipe will receive a SIGPIPE signal
/// that, by default, terminates it immediately. This can be convenient for
/// simple CLI tools used in shell pipelines, where this is a common error that
/// is best handled by exiting silently, unlike most other errors that should be
/// reported to the user. However, this default can be problematic for more
/// complex applications, especially networked servers that could write to a
/// broken pipe any time a client unexpectedly closes or drops a connection.
///
/// Unlike C programs, Rust programs on Unix(-like) systems ignore SIGPIPE by
/// default, and instead return a normal [`Result::Err`] for writes to broken
/// pipes. While there are good reasons that this is *not* actually a great
/// default, it provides valuable consistency with non-Unix platforms and is
/// helpful in the server use case described above. Unfortunately, experience
/// shows that real-world Rust libraries do not always propagate I/O errors in a
/// manner that makes them easy to consistently identify, e.g. by including I/O
/// error values in error source chains. This can make it difficult to implement
/// the desirable CLI behavior of exiting silently on broken pipes while loudly
/// reporting other errors.
///
/// `Writer` provides the benefits of the default Unix SIGPIPE behavior in a
/// more Rust-friendly and cross-platform manner. On Unix(-like) systems in
/// particular, `Writer` terminates the program using the default SIGPIPE
/// handler for full consistency with typical Unix CLI tools, but without
/// modifying global SIGPIPE behavior up front in a way that might conflict with
/// other use cases.
///
/// For further background on Rust's handling of SIGPIPE, see:
///
/// - <https://github.com/rust-lang/rust/issues/62569>
/// - <https://stackoverflow.com/a/65760807>
/// - <https://github.com/BurntSushi/ripgrep/issues/200#issuecomment-616884727>
pub(crate) struct Writer<W>(W)
where
	W: Write;

impl<W> Writer<W>
where
	W: Write,
{
	pub(crate) fn new(w: W) -> Writer<W> {
		Writer(w)
	}
}

impl<W> Write for Writer<W>
where
	W: Write,
{
	fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
		check_for_broken_pipe(self.0.write(buf))
	}

	fn flush(&mut self) -> io::Result<()> {
		check_for_broken_pipe(self.0.flush())
	}

	// NOTE: This covers additional trait methods that are stable in Rust 1.61
	// (xt's MSRV as of this writing). More may be added in the future.

	fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
		check_for_broken_pipe(self.0.write_all(buf))
	}

	fn write_fmt(&mut self, fmt: std::fmt::Arguments<'_>) -> io::Result<()> {
		check_for_broken_pipe(self.0.write_fmt(fmt))
	}

	fn write_vectored(&mut self, bufs: &[io::IoSlice<'_>]) -> io::Result<usize> {
		check_for_broken_pipe(self.0.write_vectored(bufs))
	}
}

fn check_for_broken_pipe<T>(result: io::Result<T>) -> io::Result<T> {
	match result {
		Err(err) if err.kind() == io::ErrorKind::BrokenPipe => exit_for_broken_pipe(),
		result => result,
	}
}

fn exit_for_broken_pipe() -> ! {
	#[cfg(unix)]
	// SAFETY: These are FFI calls to libc, which we assume is implemented
	// correctly. Because everything in the block comes from libc, there are no
	// Rust invariants to violate.
	unsafe {
		libc::signal(libc::SIGPIPE, libc::SIG_DFL);
		libc::raise(libc::SIGPIPE);
	}

	// Non-Unix systems fall back to a normal silent exit (and Unix systems
	// should not reach this line).
	std::process::exit(1);
}
