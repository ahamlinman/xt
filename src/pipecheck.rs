use std::io::{self, Write};

/// A writer that exits the program on broken pipe errors.
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
