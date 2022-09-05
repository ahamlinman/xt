//! Generic support for input from both slice and reader sources.
//!
//! This module provides the abstractions that enable xt to dynamically select
//! the most functional and efficient translation strategy for any given input
//! regardless of how the tool is invoked. Several factors have influenced its
//! design:
//!
//! - To translate unbounded streams of input documents, we must be able to
//!   provide a reader to the input format's deserializer.
//!
//! - To support format detection based on parser trials without sacrificing
//!   general reader support, we must be able to "rewind" a potentially
//!   non-seekable input.
//!
//! - However, the ability to rewind a reader should not impose any extra cost
//!   in cases where format detection is unnecessary.
//!
//! - Even for input formats that support readers, it is almost always faster to
//!   translate from an in-memory slice of the full input when one is readily
//!   available.
//!
//! - Not all input formats can translate from a reader, and must consume their
//!   input from an in-memory slice no matter what.
//!
//! The module accomplishes this by supporting both slice and reader inputs,
//! exposing both possibilities to formats that can optimize for each one, and
//! providing convenience methods for slice-only formats that automatically
//! consume reader input as necessary.
//!
//! For slice inputs, the module simply passes the borrowed slice directly to
//! the format.
//!
//! For reader inputs, borrowing the input for format detection produces a
//! special reader that captures all bytes consumed from the source, and that
//! rewinds before each subsequent use to produce those bytes again before
//! consuming more from the source. If a format happens to consume all of a
//! source reader while borrowing it, or if it requests slice-based input, all
//! future use of the input will become slice-based. When the selected input
//! format is ready to take ownership of the input, it can receive a byte
//! buffer, a chain of the prefix and source readers, or the source reader alone
//! in the event that format detection was skipped.

use std::borrow::Cow;
use std::io::{self, Cursor, Read, Write};

/// A container for input to [`xt::translate`][crate::translate].
///
/// xt accepts input from both slices and readers, and will produce consistent
/// output for a given input regardless of its source. However, xt may optimize
/// its behavior or provide features depending on the kind of source used. See
/// [`Handle::from_slice`] and [`Handle::from_reader`] for details.
pub struct Handle<'i>(Source<'i>);

/// The private container for the original input a [`Handle`] was created from.
enum Source<'i> {
	Slice(&'i [u8]),
	Reader(GuardedCaptureReader<Box<dyn Read + 'i>>),
}

impl<'i> Handle<'i> {
	/// Creates a handle for an input slice.
	///
	/// Slice inputs are typically more efficient to translate than reader
	/// inputs, but require all input to be loaded into memory in advance. This
	/// may be inappropriate for an unbounded stream of documents in a format
	/// that supports streaming translation.
	pub fn from_slice(b: &'i [u8]) -> Handle<'i> {
		Handle(Source::Slice(b))
	}

	/// Creates a handle for an input reader.
	///
	/// Reader inputs enable streaming translation for select input formats,
	/// allowing xt to translate documents as they appear in the stream without
	/// buffering more than one document in memory at a time. When translating
	/// from a format that does not support streaming, xt will buffer the entire
	/// contents of the reader into memory before starting translation.
	pub fn from_reader<R>(r: R) -> Handle<'i>
	where
		R: Read + 'i,
	{
		Handle(Source::Reader(GuardedCaptureReader::new(Box::new(r))))
	}

	/// Borrows a temporary reference to the input.
	///
	/// For slice inputs, this provides access to the original slice.
	///
	/// For reader inputs that are fully buffered from previous use of the
	/// handle, this provides access to the reader's full contents as a slice.
	///
	/// For reader inputs not yet fully buffered, this provides access to the
	/// reader through a wrapper that captures its output. In subsequent calls
	/// to `borrow_mut`, the wrapper will produce the captured bytes before
	/// producing more bytes from the original reader.
	pub(crate) fn borrow_mut(&mut self) -> Ref<'i, '_> {
		match &mut self.0 {
			Source::Slice(b) => Ref::Slice(b),
			Source::Reader(r) => {
				let r = r.rewind_and_borrow_mut();
				if r.is_source_eof() {
					Ref::Slice(r.captured())
				} else {
					Ref::Reader(r)
				}
			}
		}
	}
}

/// Produces the original input as a slice, either by passing through the
/// original slice or fully reading the original reader into a buffer.
impl<'i> TryFrom<Handle<'i>> for Cow<'i, [u8]> {
	type Error = io::Error;

	fn try_from(handle: Handle<'i>) -> io::Result<Cow<'i, [u8]>> {
		match handle.0 {
			Source::Slice(b) => Ok(Cow::Borrowed(b)),
			Source::Reader(r) => {
				let mut r = r.rewind_and_take();
				r.capture_to_end()?;
				let (cursor, _) = r.into_inner();
				return Ok(Cow::Owned(cursor.into_inner()));
			}
		}
	}
}

/// A container for owned input obtained by consuming a [`Handle`].
///
/// The kind of `Input` produced from a `Handle` may not correspond directly to
/// the original source. If a reader input was fully buffered through use of the
/// `Handle`, the `Input` will provide ownership of the buffer, and the
/// conversion from `Handle` will drop the reader.
pub(crate) enum Input<'i> {
	Slice(Cow<'i, [u8]>),
	Reader(Box<dyn Read + 'i>),
}

impl<'i> From<Handle<'i>> for Input<'i> {
	fn from(handle: Handle<'i>) -> Self {
		match handle.0 {
			Source::Slice(b) => Input::Slice(Cow::Borrowed(b)),
			Source::Reader(r) => {
				let r = r.rewind_and_take();
				let source_eof = r.is_source_eof();
				let (cursor, source) = r.into_inner();
				if source_eof {
					Input::Slice(Cow::Owned(cursor.into_inner()))
				} else if cursor.get_ref().is_empty() {
					Input::Reader(source)
				} else {
					Input::Reader(Box::new(cursor.chain(source)))
				}
			}
		}
	}
}

/// A temporary reference to input from a [`Handle`].
///
/// See [`Handle::borrow_mut`] for details.
pub(crate) enum Ref<'i, 'h>
where
	'i: 'h,
{
	Slice(&'h [u8]),
	Reader(&'h mut CaptureReader<Box<dyn Read + 'i>>),
}

impl<'i, 'h> Ref<'i, 'h>
where
	'i: 'h,
{
	/// Returns the full input as a slice.
	///
	/// For reader inputs not yet fully buffered, this will immediately consume
	/// all remaining bytes from the reader into memory.
	pub(crate) fn slice(&mut self) -> io::Result<&[u8]> {
		match self {
			Ref::Slice(b) => Ok(b),
			Ref::Reader(r) => {
				r.capture_to_end()?;
				Ok(r.captured())
			}
		}
	}

	/// Returns a prefix of the input.
	///
	/// For slice inputs and fully buffered reader inputs, this simply returns
	/// the full input.
	///
	/// For reader inputs not yet fully buffered, `size_hint` represents the
	/// minimum size of the prefix that the call should attempt to produce by
	/// capturing new bytes from the source if necessary. The returned prefix
	/// may be smaller or larger than `size_hint` if the reader reaches EOF or
	/// more input is already captured.
	pub(crate) fn prefix(&mut self, size_hint: usize) -> io::Result<&[u8]> {
		match self {
			Ref::Slice(b) => Ok(b),
			Ref::Reader(r) => {
				r.capture_up_to_size(size_hint)?;
				Ok(r.captured())
			}
		}
	}
}

/// A wrapper that forces a [`CaptureReader`] to be rewound prior to use.
///
/// This is designed to statically prevent bugs caused by forgetting to rewind
/// the reader before exposing its contents to consumers.
struct GuardedCaptureReader<R>(CaptureReader<R>)
where
	R: Read;

impl<R> GuardedCaptureReader<R>
where
	R: Read,
{
	/// Returns a new `RewindGuard` containing a new `CaptureReader`.
	fn new(r: R) -> Self {
		Self(CaptureReader::new(r))
	}

	/// Rewinds the `CaptureReader`, then returns a mutable reference.
	fn rewind_and_borrow_mut(&mut self) -> &mut CaptureReader<R> {
		self.0.rewind();
		&mut self.0
	}

	/// Rewinds the `CaptureReader`, then returns it.
	fn rewind_and_take(mut self) -> CaptureReader<R> {
		self.0.rewind();
		self.0
	}
}

/// A wrapper that captures the output of a reader into a buffer as it is read.
///
/// A `CaptureReader` provides limited seeking capabilities for a non-seekable
/// reader by storing a copy of the bytes it produces for each `read` call.
/// After calling [`rewind`][CaptureReader::rewind], the `CaptureReader` will
/// produce the stored bytes for new `read` calls before consuming more of the
/// source, as if [`Seek::rewind`][std::io::Seek::rewind] had been used on the
/// source (except that rewinding a `CaptureReader` is infallible).
///
/// A `CaptureReader` also tracks when the source reports an "end of file"
/// condition, indicating that the source is fully buffered as if
/// [`read_to_end`][Read::read_to_end] had been used. Consuming the
/// `CaptureReader` with [`into_inner`][CaptureReader::into_inner] permits
/// access to the buffered bytes without additional copies.
pub(crate) struct CaptureReader<R>
where
	R: Read,
{
	prefix: Cursor<Vec<u8>>,
	source: R,
	source_eof: bool,
}

impl<R> CaptureReader<R>
where
	R: Read,
{
	/// Creates a new reader that captures the bytes produced by `source`.
	fn new(source: R) -> Self {
		Self {
			prefix: Cursor::new(vec![]),
			source,
			source_eof: false,
		}
	}

	/// Returns a slice of all input captured by the reader so far.
	fn captured(&self) -> &[u8] {
		self.prefix.get_ref()
	}

	/// Returns the number of bytes remaining to read from the captured prefix
	/// before consuming more from the source.
	fn captured_unread_size(&self) -> usize {
		// The cursor position is relative to an in-memory slice. This shouldn't
		// truncate unless we manually give the cursor a ridiculous position.
		#[allow(clippy::cast_possible_truncation)]
		let offset = self.prefix.position() as usize;
		self.prefix.get_ref().len() - offset
	}

	/// Rewinds the reader so that subsequent reads will produce the source's
	/// bytes from the beginning.
	fn rewind(&mut self) {
		self.prefix.set_position(0);
	}

	/// Captures all of the source's remaining input without modifying the
	/// reader's position.
	fn capture_to_end(&mut self) -> io::Result<()> {
		if !self.source_eof {
			self.source.read_to_end(self.prefix.get_mut())?;
			self.source_eof = true;
		}
		Ok(())
	}

	/// Attempts to read enough data from the source for the capture buffer to
	/// contain at least `size` bytes, without modifying the reader's position.
	///
	/// The actual number of captured bytes may be less than `size` if the
	/// source reaches EOF before producing `size` bytes, or more than `size` if
	/// more of the source is already captured.
	fn capture_up_to_size(&mut self, size: usize) -> io::Result<()> {
		let needed = size.saturating_sub(self.prefix.get_ref().len());
		if needed == 0 {
			return Ok(());
		}

		let mut take = (&mut self.source).take(needed as u64);
		take.read_to_end(self.prefix.get_mut())?;
		if take.limit() > 0 {
			self.source_eof = true;
		}
		Ok(())
	}

	/// Returns true if the latest read from the source indicated an EOF.
	fn is_source_eof(&self) -> bool {
		self.source_eof
	}

	/// Consumes the reader, returning any captured prefix as well as the
	/// source.
	fn into_inner(self) -> (Cursor<Vec<u8>>, R) {
		(self.prefix, self.source)
	}
}

impl<R> Read for CaptureReader<R>
where
	R: Read,
{
	fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
		// First, copy as much data as we can from the unread portion of the
		// cursor into the buffer.
		let prefix_size = std::cmp::min(buf.len(), self.captured_unread_size());
		self.prefix.read_exact(&mut buf[..prefix_size])?;
		if self.captured_unread_size() > 0 || prefix_size == buf.len() {
			return Ok(prefix_size);
		}

		// Second, fill the rest of the buffer with data from the source, and
		// capture it for ourselves as well.
		//
		// The `read` documentation recommends against us reading from `buf`,
		// but does not prevent it, and does require callers of `read` to assume
		// we might do this. As morally questionable as it is, this approach
		// lets our consumer drive the number and size of reads against the
		// source, making our presence more transparent to both sides. As the
		// smallest consolation, it's worth noting that we only read bytes we
		// know were freshly written, and do not rely on the original contents
		// of `buf` in any way (unless, of course, the source is broken and lies
		// about how many bytes it read).
		let buf = &mut buf[prefix_size..];
		let source_size = self.source.read(buf)?;
		self.prefix.write_all(&buf[..source_size])?;

		// Finally, mark whether the source is at EOF (keeping in mind that it
		// can, in theory, return more data after an earlier EOF). We know `buf`
		// can't be empty as we return early when `prefix_size == buf.len()`, so
		// a 0 byte read can only indicate EOF.
		self.source_eof = source_size == 0;

		Ok(prefix_size + source_size)
	}
}

#[cfg(test)]
mod tests {
	use super::{CaptureReader, Handle, Input, Ref};
	use std::borrow::Cow;
	use std::io::{self, Cursor, Read};

	const DATA: &str = "abcdefghij";
	const HALF: usize = DATA.len() / 2;

	#[test]
	fn input_borrow_mut_rewind() {
		let mut handle = Handle::from_reader(DATA.as_bytes());
		let mut buf = vec![];

		let mut input_ref = handle.borrow_mut();
		match input_ref {
			Ref::Slice(_) => unreachable!(),
			Ref::Reader(ref mut r) => r.take(HALF as u64).read_to_end(&mut buf).unwrap(),
		};
		assert_eq!(std::str::from_utf8(&buf), Ok(&DATA[..HALF]));
		buf.clear();

		// `Ref`s are designed to be forgettable without breaking behavior. Part
		// of the intent of this test is to ensure that no future `Drop` impl
		// breaks this expectation.
		#[allow(clippy::forget_non_drop)]
		std::mem::forget(input_ref);

		match handle.borrow_mut() {
			Ref::Slice(_) => unreachable!(),
			Ref::Reader(r) => r.take(HALF as u64).read_to_end(&mut buf).unwrap(),
		};
		assert_eq!(std::str::from_utf8(&buf), Ok(&DATA[..HALF]));
		buf.clear();

		// If we only consume part of a borrowed reader, we need to reset the
		// reader before giving ownership away.
		let mut r = match handle.into() {
			Input::Slice(_) => unreachable!(),
			Input::Reader(r) => r,
		};
		assert!(matches!(r.read_to_end(&mut buf), Ok(len) if len == DATA.len()));
		assert_eq!(std::str::from_utf8(&buf), Ok(DATA));
	}

	#[test]
	fn input_into_cow() {
		let mut handle = Handle::from_reader(DATA.as_bytes());

		match handle.borrow_mut() {
			Ref::Slice(_) => unreachable!(),
			Ref::Reader(r) => io::copy(&mut r.take(HALF as u64), &mut io::sink()).unwrap(),
		};

		// If we only consume part of a borrowed reader, turning the input into
		// a slice should still produce the full input.
		let buf: Cow<'_, [u8]> = handle.try_into().unwrap();
		assert_eq!(std::str::from_utf8(&buf), Ok(DATA));
	}

	#[test]
	fn rewindable_reader_straight_read() {
		let mut r = CaptureReader::new(Cursor::new(String::from(DATA)));

		let mut result = String::new();
		assert!(matches!(r.read_to_string(&mut result), Ok(len) if len == DATA.len()));
		assert_eq!(result, DATA);
		assert!(r.is_source_eof());

		let (cursor, _) = r.into_inner();
		assert!(matches!(std::str::from_utf8(cursor.get_ref()), Ok(DATA)));
	}

	#[test]
	fn rewinding_rewindable_reader() {
		let mut r = CaptureReader::new(Cursor::new(String::from(DATA)));

		let mut tmp = [0; HALF];
		assert!(matches!(r.read_exact(&mut tmp), Ok(())));
		assert_eq!(std::str::from_utf8(&tmp), Ok(&DATA[..HALF]));
		assert_eq!(std::str::from_utf8(r.captured()), Ok(&DATA[..HALF]));
		assert!(!r.is_source_eof());

		r.rewind();

		let mut result = String::new();
		assert!(matches!(r.read_to_string(&mut result), Ok(len) if len == DATA.len()));
		assert_eq!(result, DATA);
		assert_eq!(r.captured(), DATA.as_bytes());
		assert!(r.is_source_eof());
	}

	#[test]
	fn rewindable_reader_capture_to_end() {
		let mut r = CaptureReader::new(Cursor::new(String::from(DATA)));
		assert!(matches!(r.capture_to_end(), Ok(_)));
		assert_eq!(std::str::from_utf8(r.captured()), Ok(DATA));
		assert!(r.is_source_eof());
	}

	#[test]
	fn rewindable_reader_capture_up_to() {
		let mut r = CaptureReader::new(Cursor::new(String::from(DATA)));
		assert!(matches!(r.capture_up_to_size(HALF), Ok(_)));
		assert_eq!(std::str::from_utf8(r.captured()), Ok(&DATA[..HALF]));
		assert!(!r.is_source_eof());
	}
}
