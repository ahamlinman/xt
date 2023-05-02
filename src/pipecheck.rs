//! Cross-platform Unix-style handling of broken pipe errors.

use std::io::{self, Write};

/// A writer that silently exits the program on broken pipe errors.
///
/// When a `Writer`'s underlying writer returns [`io::ErrorKind::BrokenPipe`] in
/// response to any [`Write`] method, `Writer` will immediately terminate the
/// program without returning from the call. On Unix(-like) systems, the program
/// will terminate as if killed by the default SIGPIPE handler. Otherwise, it
/// will simply exit with return code 1.
///
/// # Why is this useful?
///
/// A Unix program that writes to a broken pipe will receive a SIGPIPE signal
/// that, by default, causes it to exit immediately. This can be convenient for
/// simple CLI tools intended for use in shell pipelines, especially since these
/// errors are very common and are best handled by exiting silently (unlike most
/// other errors, including most other I/O errors). However, this behavior can
/// be problematic for more complex applications, particularly networked servers
/// that could write to a broken pipe any time a client unexpectedly closes or
/// drops a connection.
///
/// Unlike C programs, Rust programs on Unix(-like) systems ignore SIGPIPE by
/// default, and instead return a normal error value for writes to broken pipes.
/// While there is debate about this choice, it is not inherently unreasonable,
/// especially for the sake of compatibility with non-Unix platforms and for the
/// server use case described above. Unfortunately, experience shows that
/// real-world Rust libraries do not always propagate I/O errors in a manner
/// that makes them easy to consistently identify, e.g. by including I/O error
/// values in error source chains. This can make it difficult to implement the
/// desirable CLI behavior of exiting silently on broken pipes while loudly
/// reporting other errors.
///
/// `Writer` provides the benefits of the default Unix SIGPIPE behavior in a
/// more Rust-friendly and cross-platform manner. On Unix(-like) systems in
/// particular, `Writer` terminates the program using the default SIGPIPE
/// handler for full consistency with "typical" Unix CLI tools, but without
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
