//! Cross-platform Unix-style handling of broken pipe errors.

use std::io::{self, Write};

/// A writer that silently terminates the program on broken pipe errors.
///
/// When any call to its underlying writer returns a [`BrokenPipe`](io::ErrorKind::BrokenPipe)
/// error, a `Writer` terminates the current program with a SIGPIPE signal,
/// or exits with code 1 on non-Unix systems.
///
/// # Why is this useful?
///
/// A Unix program that writes to a broken pipe receives a SIGPIPE signal that,
/// by default, terminates it immediately. This is usually the optimal behavior
/// for simple CLI tools used in shell pipelines, but is problematic for
/// applications like networked servers that may write to a broken pipe any time
/// a client unexpectedly closes or drops a connection.
///
/// Rust overrides the Unix default during program initialization (before `main`
/// is called), ignoring SIGPIPE so that writes to broken pipes consistently
/// return a normal [`Result::Err`]. This is helpful for the server use case
/// described above and provides valuable consistency with non-Unix platforms,
/// even though it severely hampers some specialized use cases on Unix.
///
/// Unfortunately, experience shows that real-world Rust libraries don't
/// consistently propagate broken pipe errors in a generically detectable way,
/// such as by including them in error source chains. This makes it difficult
/// for well-meaning CLIs to replicate the behavior of Unix's default SIGPIPE
/// handler in a cross-platform manner. `Writer` solves this by plumbing the
/// required termination logic directly into every write operation, before it
/// can get lost on its way up the call stack.
///
/// On Unix systems in particular, `Writer` accomplishes this by modifying the
/// process-wide behavior of SIGPIPE immediately before raising that signal
/// against the current thread, rather than requiring its modification for all
/// writes in the program. It is also important to note that Unix distinguishes
/// between process termination by a signal and process termination with an exit
/// code; `Writer` does not employ incorrect hacks like exiting with code 141
/// (attempting to replicate the return code of a command terminated by SIGPIPE
/// in a shell).
///
/// For further background on SIGPIPE, Rust's handling of it, and cross-platform
/// portability concerns surrounding broken pipes, see:
///
/// - <https://github.com/rust-lang/rust/issues/62569>
/// - <https://stackoverflow.com/a/65760807>
/// - <https://github.com/BurntSushi/ripgrep/issues/200#issuecomment-616884727>
///
/// For the implementation in the Go programming language that directly inspired
/// this approach, see:
///
/// - <https://pkg.go.dev/os/signal#hdr-SIGPIPE>
/// - <https://cs.opensource.google/go/go/+/refs/tags/go1.23.6:src/os/file_unix.go;l=252>
/// - <https://cs.opensource.google/go/go/+/refs/tags/go1.23.6:src/runtime/signal_unix.go;l=333>
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

	// NOTE: This covers additional trait methods that are stable in Rust 1.70
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
