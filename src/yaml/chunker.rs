//! Support for splitting YAML 1.2 streams into their constituent documents.
//!
//! This is an awful hack to provide some level of streaming input support atop
//! `serde_yaml`, which as of this writing requires buffering all input before
//! parsing it (the convenience methods that parse from readers simply do this
//! buffering for you). Using the same underlying parser as `serde_yaml`—a
//! version of libyaml translated from C to Rust—a [`Chunker`] iterates over the
//! documents in a YAML stream as `String`s, which can be provided one by one to
//! `serde_yaml` for actual deserialization.
//!
//! I sincerely hope that I will someday have the time and energy to implement
//! true streaming support in `serde_yaml` itself (unless, of course, someone
//! beats me to it), and that everything I've learned from this implementation
//! will serve as a stepping stone toward that.

use std::error::Error;
use std::ffi::{c_void, CStr};
use std::fmt::Display;
use std::io::{self, Read};
use std::mem::{self, MaybeUninit};
use std::ops::Deref;
use std::ptr;

use unsafe_libyaml::{
	yaml_event_delete, yaml_event_t, yaml_mark_t, yaml_parser_delete, yaml_parser_initialize,
	yaml_parser_parse, yaml_parser_set_encoding, yaml_parser_set_input, yaml_parser_t,
	YAML_DOCUMENT_END_EVENT, YAML_DOCUMENT_START_EVENT, YAML_MAPPING_START_EVENT,
	YAML_SCALAR_EVENT, YAML_SEQUENCE_START_EVENT, YAML_STREAM_END_EVENT, YAML_UTF8_ENCODING,
};

/// An iterator over individual raw documents in a UTF-8-encoded YAML stream.
pub(super) struct Chunker<R>
where
	R: Read,
{
	parser: *mut yaml_parser_t,
	read_state: *mut ReadState<R>,
	last_document: Option<Document>,
	current_document_kind: Option<DocumentKind>,
	stream_ended: bool,
}

/// The state for the libyaml input callback.
struct ReadState<R>
where
	R: Read,
{
	reader: ChunkReader<R>,
	buffer: Vec<u8>,
	error: Option<io::Error>,
}

impl<R> Chunker<R>
where
	R: Read,
{
	/// Creates a new chunker for the YAML stream produced by the reader.
	///
	/// YAML 1.2 allows several different text encodings for YAML streams, as
	/// well as the presence of byte order marks at the start of the stream or
	/// individual documents. However, `Chunker` requires a UTF-8 stream without
	/// BOMs. Consider using the [`encoding`](super::encoding) module to
	/// re-encode non-UTF-8 streams.
	pub(super) fn new(reader: R) -> Self {
		let mut parser = Box::new(MaybeUninit::<yaml_parser_t>::uninit());
		// SAFETY: libyaml functions are assumed to work correctly.
		if unsafe { yaml_parser_initialize(parser.as_mut_ptr()) }.fail {
			panic!("out of memory for libyaml parser initialization");
		}

		// NOTE: Nothing after this point is expected to panic or fail, so we
		// should not need to worry about leaking this memory before we have a
		// chance to construct the return value.
		let parser = Box::into_raw(parser).cast::<yaml_parser_t>();
		let read_state = Box::into_raw(Box::new(ReadState {
			reader: ChunkReader::new(reader),
			buffer: vec![],
			error: None,
		}));

		// SAFETY: libyaml functions are assumed to work correctly. As required
		// by `read_handler`, the data pointer points to a `ReadState<R>`.
		unsafe { yaml_parser_set_input(parser, Self::read_handler, read_state.cast::<c_void>()) };

		// SAFETY: libyaml functions are assumed to work correctly.
		unsafe { yaml_parser_set_encoding(parser, YAML_UTF8_ENCODING) };

		Self {
			parser,
			read_state,
			last_document: None,
			current_document_kind: None,
			stream_ended: false,
		}
	}

	/// Implements [`yaml_read_handler_t`](unsafe_libyaml::yaml_read_handler_t).
	///
	/// # Safety
	///
	/// The `data` pointer provided to [`yaml_parser_set_input`] alongside this
	/// function must be a valid pointer to an initialized `ReadState<R>`.
	unsafe fn read_handler(
		read_state: *mut c_void,
		buffer: *mut u8,
		size: u64,
		size_read: *mut u64,
	) -> i32 {
		const READ_SUCCESS: i32 = 1;
		const READ_FAILURE: i32 = 0;

		let read_state = read_state.cast::<ReadState<R>>();

		// `size` represents the size of an in-memory buffer, which cannot
		// possibly exceed usize::MAX.
		#[allow(clippy::cast_possible_truncation)]
		let size = size as usize;

		// Manual review shows that libyaml uses `std::alloc::alloc` to allocate
		// the provided buffer, and performs no explicit initialization of its
		// own. Because `alloc` does not necessarily initialize memory, it would
		// be instant Undefined Behavior to form a Rust slice from this buffer,
		// and even if it weren't it would be unsound to expose this buffer to a
		// safe `Read` implementation. To ensure soundness, we maintain our own
		// initialized buffer for the reader to populate, then copy that buffer
		// ourselves to libyaml.
		//
		// SAFETY: Our caller is responsible for the validity of `read_state`.
		unsafe { (*read_state).buffer.resize(size, 0) };

		// SAFETY: Our caller is responsible for the validity of `read_state`.
		match unsafe { (*read_state).reader.read(&mut (*read_state).buffer[..]) } {
			Ok(len) => {
				// SAFETY: The two buffers come from separate allocations, so
				// they cannot overlap unless the allocator is seriously broken.
				// Our caller is responsible for the validity of `read_state`.
				unsafe { ptr::copy_nonoverlapping((*read_state).buffer.as_ptr(), buffer, len) };

				// Note that libyaml's EOF condition is the same as Rust's: set
				// `size_read` to 0 and return success.
				//
				// SAFETY: libyaml is assumed to initialize the pointer correctly.
				unsafe { *size_read = len as u64 };

				// SAFETY: Our caller is responsible for the validity of `read_state`.
				unsafe { (*read_state).error = None };
				READ_SUCCESS
			}
			Err(err) => {
				// SAFETY: Our caller is responsible for the validity of `read_state`.
				unsafe { (*read_state).error = Some(err) };
				READ_FAILURE
			}
		}
	}
}

impl<R> Iterator for Chunker<R>
where
	R: Read,
{
	type Item = io::Result<Document>;

	fn next(&mut self) -> Option<Self::Item> {
		if self.stream_ended {
			return None;
		}

		loop {
			// SAFETY: `self.parser` and `self.read_state` should have been
			// properly initialized on Chunker construction.
			let event = unsafe {
				match Event::from_parser(self.parser) {
					Ok(event) => event,
					Err(()) => {
						return Some(Err((*self.read_state).error.take().unwrap_or_else(|| {
							io::Error::new(
								io::ErrorKind::InvalidData,
								ParserError::from_parser(self.parser),
							)
						})))
					}
				}
			};

			// Note that while we chunk the document as soon as we receive a
			// DOCUMENT_END event, we don't emit the chunk until the next
			// DOCUMENT_START or STREAM_END event. libyaml can sometimes parse
			// what looks like a valid YAML document from a non-YAML input, only
			// to error out when it looks for the start of the next document.
			// This is especially problematic when the chunker's output is used
			// to determine whether an arbitrary input is valid YAML (e.g. in
			// xt's parser-based format detection).
			match event.type_ {
				YAML_DOCUMENT_START_EVENT => {
					// SAFETY: `self.read_state` should have been properly
					// initialized on Chunker construction.
					unsafe {
						let offset = event.start_mark.index;
						(*self.read_state).reader.trim_to_offset(offset);
					}
					self.current_document_kind = None;
					if let Some(doc) = self.last_document.take() {
						return Some(Ok(doc));
					}
				}
				YAML_SCALAR_EVENT => {
					self.current_document_kind
						.get_or_insert(DocumentKind::Scalar);
				}
				YAML_SEQUENCE_START_EVENT | YAML_MAPPING_START_EVENT => {
					self.current_document_kind
						.get_or_insert(DocumentKind::Collection);
				}
				YAML_DOCUMENT_END_EVENT => {
					// SAFETY: `self.read_state` should have been properly
					// initialized on Chunker construction. libyaml validates
					// that the input is UTF-8 during parsing.
					let content = unsafe {
						let offset = event.end_mark.index;
						String::from_utf8_unchecked(
							(*self.read_state).reader.take_to_offset(offset),
						)
					};
					self.last_document = Some(Document {
						content,
						kind: self.current_document_kind.take().unwrap(),
					});
				}
				YAML_STREAM_END_EVENT => {
					self.stream_ended = true;
					return self.last_document.take().map(Ok);
				}
				_ => {}
			};
		}
	}
}

impl<R> Drop for Chunker<R>
where
	R: Read,
{
	fn drop(&mut self) {
		// SAFETY: libyaml functions are assumed to work correctly.
		unsafe { yaml_parser_delete(self.parser) };

		// SAFETY: These pointers were obtained from Boxes on Chunker construction.
		unsafe {
			drop(Box::from_raw(self.parser));
			drop(Box::from_raw(self.read_state));
		}
	}
}

/// A UTF-8 encoded YAML document.
pub(super) struct Document {
	content: String,
	kind: DocumentKind,
}

/// The type of content contained in a YAML document.
pub enum DocumentKind {
	Scalar,
	Collection,
}

impl Document {
	/// Returns true if the content of the document is a scalar rather than a
	/// collection (sequence or mapping).
	pub(super) fn is_scalar(&self) -> bool {
		matches!(self.kind, DocumentKind::Scalar)
	}
}

impl Deref for Document {
	type Target = str;

	fn deref(&self) -> &Self::Target {
		&self.content
	}
}

/// A libyaml event.
struct Event(*mut yaml_event_t);

impl Event {
	/// Runs the parser and returns its next event.
	///
	/// # Safety
	///
	/// `parser` must be a valid pointer to an initialized [`yaml_parser_t`].
	unsafe fn from_parser(parser: *mut yaml_parser_t) -> Result<Event, ()> {
		let mut event = Box::new(MaybeUninit::<yaml_event_t>::uninit());
		// SAFETY: libyaml functions are assumed to work correctly. Our caller
		// is responsible for the validity of `parser`.
		if unsafe { yaml_parser_parse(parser, event.as_mut_ptr()) }.fail {
			return Err(());
		}
		Ok(Event(Box::into_raw(event).cast::<yaml_event_t>()))
	}
}

impl Deref for Event {
	type Target = yaml_event_t;

	fn deref(&self) -> &Self::Target {
		// SAFETY: This is the only place where we ever convert this pointer to
		// a reference. Because we have `&self`, Rust has already verified that
		// we're following the rules.
		unsafe { &*self.0 }
	}
}

impl Drop for Event {
	fn drop(&mut self) {
		// SAFETY: libyaml functions are assumed to work correctly.
		unsafe { yaml_event_delete(self.0) };

		// SAFETY: This pointer was obtained from a Box on Event construction.
		unsafe { drop(Box::from_raw(self.0)) };
	}
}

/// A libyaml parser error including context.
#[derive(Debug)]
struct ParserError {
	problem: Option<LocatedError>,
	context: Option<LocatedError>,
}

impl ParserError {
	/// Creates an error from the current state of a parser.
	///
	/// # Safety
	///
	/// `parser` must be a valid pointer to an initialized [`yaml_parser_t`].
	unsafe fn from_parser(parser: *mut yaml_parser_t) -> Self {
		// SAFETY: Our caller is responsible for the validity of `parser`. We
		// validate that the `description` for each `LocatedError::from_parts`
		// call is not null.
		unsafe {
			Self {
				problem: (!(*parser).problem.is_null()).then(|| {
					LocatedError::from_parts(
						(*parser).problem,
						(*parser).problem_mark,
						Some((*parser).problem_offset),
					)
				}),
				context: (!(*parser).context.is_null()).then(|| {
					LocatedError::from_parts((*parser).context, (*parser).context_mark, None)
				}),
			}
		}
	}
}

impl Error for ParserError {}

impl Display for ParserError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match &self.problem {
			None => f.write_str("unknown libyaml error"),
			Some(problem) => match &self.context {
				None => Display::fmt(problem, f),
				Some(context) => write!(f, "{problem}, {context}"),
			},
		}
	}
}

/// A libyaml parser error with location information.
#[derive(Debug)]
struct LocatedError {
	description: String,
	offset: u64,
	line: u64,
	column: u64,
}

impl LocatedError {
	/// Creates an error from portions of the error state in a parser.
	///
	/// If `override_offset` is not `None`, it will replace the byte index
	/// reported by `mark` when the error is displayed.
	///
	/// # Safety
	///
	/// `description` must be a valid pointer to a valid C string.
	unsafe fn from_parts(
		description: *const i8,
		mark: yaml_mark_t,
		override_offset: Option<u64>,
	) -> Self {
		Self {
			// SAFETY: Our caller is responsible for the validity of `description`.
			description: unsafe { CStr::from_ptr(description).to_string_lossy().into_owned() },
			line: mark.line + 1,
			column: mark.column + 1,
			offset: match mark.index > 0 {
				true => mark.index,
				false => override_offset.unwrap_or(0),
			},
		}
	}
}

impl Display for LocatedError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if self.line == 1 && self.column == 1 {
			write!(
				f,
				"{issue} at position {byte}",
				issue = self.description,
				byte = self.offset
			)
		} else {
			write!(
				f,
				"{issue} at line {line} column {column}",
				issue = self.description,
				line = self.line,
				column = self.column,
			)
		}
	}
}

/// A reader that captures bytes read from a source and provides them in chunks.
struct ChunkReader<R>
where
	R: Read,
{
	reader: R,
	captured: Vec<u8>,
	captured_start_offset: u64,
}

impl<R> ChunkReader<R>
where
	R: Read,
{
	fn new(reader: R) -> Self {
		Self {
			reader,
			captured: vec![],
			captured_start_offset: 0,
		}
	}

	/// Trims from the start of the capture buffer so the next chunk will begin
	/// at the specified reader offset.
	fn trim_to_offset(&mut self, offset: u64) {
		let trim_len = usize::try_from(offset - self.captured_start_offset).unwrap();
		self.captured_start_offset = offset;
		self.captured.drain(..trim_len);
	}

	/// Takes the chunk from the start of the capture buffer up to the specified
	/// reader offset, leaving bytes beyond the offset in the capture buffer.
	fn take_to_offset(&mut self, offset: u64) -> Vec<u8> {
		let take_len = usize::try_from(offset - self.captured_start_offset).unwrap();
		let tail = self.captured.split_off(take_len);
		self.captured_start_offset = offset;
		mem::replace(&mut self.captured, tail)
	}
}

impl<R> Read for ChunkReader<R>
where
	R: Read,
{
	fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
		// While the `read` documentation recommends against reading from `buf`,
		// it does not prevent it, and does require callers of `read` to assume
		// we might do this. As consolation, note that we only read back bytes
		// that we know were freshly written, unless of course the source is
		// broken and lies about how many bytes it read.
		let len = self.reader.read(buf)?;
		self.captured.extend_from_slice(&buf[..len]);
		Ok(len)
	}
}
