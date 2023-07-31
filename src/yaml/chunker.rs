//! Support for splitting YAML 1.2 streams into their constituent documents.
//!
//! This is an awful hack to provide some level of streaming input support atop
//! `serde_yaml`, which as of this writing requires buffering all input before
//! parsing it (the convenience methods that parse from readers simply do this
//! buffering for you). Using the same underlying parser as `serde_yaml` (a Rust
//! translation of the venerable [libyaml][libyaml]), a [`Chunker`] iterates
//! over the documents in a YAML stream as `String`s, which can be provided one
//! by one to `serde_yaml` for actual deserialization.
//!
//! I sincerely hope that I will someday have the time and energy to implement
//! true streaming support in `serde_yaml` itself (unless, of course, someone
//! beats me to it), and that this implementation will serve as a stepping stone
//! toward that goal.
//!
//! [libyaml]: https://pyyaml.org/wiki/LibYAML

use std::error::Error;
use std::ffi::{c_char, c_void, CStr};
use std::fmt::Display;
use std::io::{self, Read};
use std::mem::{self, MaybeUninit};
use std::ops::Deref;
use std::ptr;

// LIBYAML SAFETY NOTE: The unsafe_libyaml crate is a largely mechanical C to
// Rust translation of the venerable libyaml (see the module documentation).
// While it is technically Rust code, its API is roughly the same as the _real_
// libyaml called through FFI, including the fact that every function is
// considered inherently unsafe.
//
// Given its C heritage, unsafe_libyaml (which we'll just call "libyaml" for
// brevity) does not document safety expectations as comprehensively as typical
// Rust code. For the most part, we simply assume that libyaml works "as
// expected," whatever that happens to mean in context. We'll try to document
// some of the more notable things we do and don't assume about libyaml where it
// makes sense to do so.
//
// We implicitly assume that in all cases where a libyaml function accepts a
// pointer argument, this pointer must be non-null, aligned for the pointee
// type, dereferenceable (i.e. pointing within a single allocated object), and
// live (i.e. not pointing to deallocated memory). We assume that initialization
// functions may safely accept pointers to uninitialized memory, and that
// pointers passed to all other functions must point to memory that is logically
// initialized for the pointee type. We implicitly make the same assumptions of
// pointers that libyaml returns, passes, or exposes to us.
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
	// Note that anything we need libyaml to access is kept as raw pointers,
	// since otherwise the uniqueness requirements of Box<T> make it too easy to
	// violate Stacked Borrows (e.g. pulling a chunk out of self.read_state must
	// not invalidate the data pointer that libyaml keeps for its read handler).
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

		// SAFETY: The call to yaml_parser_initialize is unsafe; see the LIBYAML
		// SAFETY NOTE. The pointer we pass is non-null, dereferenceable,
		// aligned, and live by virtue of the MaybeUninit being within a live
		// Box (though the pointed-to memory is uninitialized as we expect
		// libyaml to initialize it).
		if unsafe { yaml_parser_initialize(parser.as_mut_ptr()) }.fail {
			panic!("out of memory for libyaml parser initialization");
		}

		// PARSER POINTER NOTE: This pointer meets the requirements for
		// non-initialization libyaml functions defined in the LIBYAML SAFETY
		// NOTE. It is non-null, aligned, dereferenceable, and live due to its
		// allocation from a Box. Furthermore, since we did not panic above, we
		// know that the pointee yaml_parser_t is properly initialized.
		//
		// Note that this cast is like a hidden MaybeUninit::assume_init. We
		// expect this to be fine as MaybeUninit<T> is guaranteed to have the
		// same layout as T, and because we successfully initialized the value
		// as mentioned above.
		let parser = Box::into_raw(parser).cast::<yaml_parser_t>();

		// SAFETY: The call to yaml_parser_set_encoding is unsafe; see the
		// LIBYAML SAFETY NOTE and PARSER POINTER NOTE. No special invariants
		// are expected in relation to the u32 enum parameter that specifies the
		// text encoding of the input.
		unsafe { yaml_parser_set_encoding(parser, YAML_UTF8_ENCODING) };

		// READ STATE POINTER NOTE: This pointer meets the requirements for
		// non-initialization libyaml functions defined in the LIBYAML SAFETY
		// NOTE. It is non-null, aligned, dereferenceable, and live due to its
		// allocation from a Box. Furthermore, the pointee ReadState is
		// initialized by Rust before being moved into the Box.
		let read_state = Box::into_raw(Box::new(ReadState {
			reader: ChunkReader::new(reader),
			buffer: vec![],
			error: None,
		}));

		// SAFETY: There are, in a sense, two unsafe operations here: calling
		// yaml_parser_set_input, and passing the unsafe Self::read_handler
		// function (which is not technically unsafe on its own, but will result
		// in calls to that unsafe function later on).
		//
		// With respect to the yaml_parser_set_input call, see the LIBYAML
		// SAFETY NOTE and PARSER POINTER NOTE.
		//
		// With respect to the use of Self::read_handler, the function is unsafe
		// as it requires that the data pointer passed to yaml_parser_set_input
		// is a valid pointer to an initialized ReadState<R>. See the READ STATE
		// POINTER NOTE for an explanation of how we satisfy this.
		unsafe { yaml_parser_set_input(parser, Self::read_handler, read_state.cast::<c_void>()) };

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
	/// When an instantiation of this function with a given `R: Read` is
	/// provided as the `handler` argument in a call to [`yaml_parser_set_input`],
	/// the `data` pointer in that call must be a valid pointer to an initialized
	/// `ReadState<R>`.
	unsafe fn read_handler(
		read_state: *mut c_void,
		buffer: *mut u8,
		buffer_size: u64,
		size_read: *mut u64,
	) -> i32 {
		const READ_SUCCESS: i32 = 1;
		const READ_FAILURE: i32 = 0;

		// SAFETY: The conversion of the read_state pointer to a &mut is unsafe.
		// To analyze its safety, we consider the validity of the pointer and
		// the validity of the resulting lifetime, particularly with respect to
		// possible aliasing.
		//
		// With respect to pointer validity, we lift this requirement to our
		// caller.
		//
		// With respect to the lifetime, the reference will be alive through the
		// end of this function. Since this binding shadows the original
		// read_state raw pointer, there should be no possibility for aliasing
		// access through both the &mut reference and the raw pointer within the
		// function itself. We assume that libyaml is single-threaded and will
		// not make concurrent calls to a read handler with the same data
		// pointer, and defer to the LIBYAML SAFETY NOTE as necessary.
		let read_state = unsafe { &mut *read_state.cast::<ReadState<R>>() };

		// Manual inspection of libyaml has shown that it uses std::alloc::alloc
		// to allocate the buffer it provides to us, with no initialization of
		// the buffer's memory. It would be instant Undefined Behavior to create
		// a Rust slice using this potentially uninitialized memory, and even if
		// it weren't it would be unsound to expose that memory to a safe Read
		// implementation, as the Read::read documentation explicitly calls out.
		// As such, we need our own initialized buffer (maintained across calls
		// for efficiency) whose contents we can copy into the libyaml buffer.
		let buffer_size = usize::try_from(buffer_size).unwrap();
		read_state.buffer.resize(buffer_size, 0);

		match read_state.reader.read(&mut read_state.buffer[..]) {
			Ok(read_len) => {
				// This is a massively important check for the safety of the
				// ptr::copy_nonoverlapping call below, since the consequence
				// would be an out-of-bounds memory write. As of this writing,
				// the safe ChunkReader implementation actually prevents this
				// from ever being true, but obviously we aren't going to rely
				// on that. Note that while we often treat libyaml as if it's
				// FFI, it's okay to unwind here as unsafe_libyaml is actually
				// pure Rust code translated from C.
				if read_len > buffer_size {
					panic!("misbehaving reader claims a {read_len} byte read into a {buffer_size} byte buffer");
				}

				// SAFETY: The call to ptr::copy_nonoverlapping is (extremely)
				// unsafe. To analyze its safety, we'll consider its four
				// documented safety requirements in turn.
				//
				// First: the pointer returned by `read_state.buffer.as_ptr()`
				// must be valid for reads of `read_len` bytes. We expect it to
				// be non-null and live by virtue of pointing to the backing
				// allocation of a live Vec<u8>. Since we resized the buffer to
				// `buffer_size` bytes before calling the reader, we expect the
				// pointer to be valid for reads of `buffer_size` bytes. Because
				// we panic when `read_len > buffer_size`, we can guarantee here
				// that `read_len <= buffer_size`, and that a pointer valid for
				// reads of `buffer_size` bytes is therefore valid for reads of
				// `read_len` bytes.
				//
				// Second: the `buffer` pointer must be valid for writes of
				// `read_len` bytes. This pointer is provided by libyaml, along
				// with the corresponding `buffer_size` that represents the
				// maximum size for which `buffer` is valid for writes. We note
				// once again that `read_len <= buffer_size`, and that a pointer
				// valid for writes of `buffer_size` bytes must therefore be
				// valid for writes of `read_len` bytes. We otherwise largely
				// defer to the LIBYAML SAFETY NOTE, however as an additional
				// check on libyaml we explicitly validate that `buffer` is
				// non-null.
				//
				// Third: Both the source and destination pointers must be
				// properly aligned. The pointee type of both pointers is u8,
				// whose size is guaranteed by definition to be 1 byte. Because
				// "the size of a value is always a multiple of its alignment"
				// (per the "Type Layout" section of the Rust Reference), 1 must
				// be a multiple of the alignment of a u8, which means that the
				// alignment of a u8 must itself be 1. As such, any non-null u8
				// pointer is inherently aligned.
				//
				// Fourth: The region of memory beginning at the source pointer
				// with a size of `read_len` bytes must not overlap with the
				// region of memory beginning at the destination pointer with
				// the same size. We expect the global allocator to satisfy this
				// property on our behalf, as the buffer provided by libyaml and
				// the buffer that we manage within the read state are distinct
				// allocations.
				unsafe {
					if !buffer.is_null() {
						ptr::copy_nonoverlapping(read_state.buffer.as_ptr(), buffer, read_len);
					}
				}

				// SAFETY: The dereference of `size_read` is unsafe. Since
				// libyaml provides this pointer, we largely defer to the
				// LIBYAML SAFETY NOTE. However, as an additional check on
				// libyaml, we've explicitly validated that `size_read` is
				// non-null. Note that because `u64: Copy`, and `Drop` is
				// mutually exclusive with `Copy`, there is no danger of us
				// dropping uninitialized memory during the assignment.
				unsafe {
					if !size_read.is_null() {
						*size_read = read_len as u64;
					}
					// Note that EOF does not require special handling, as
					// libyaml's EOF condition is the same as Rust's: report a
					// successful read of 0 bytes.
				}

				read_state.error = None;
				READ_SUCCESS
			}
			Err(err) => {
				read_state.error = Some(err);
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
			// SAFETY: The call to Event::from_parser is unsafe, as it requires
			// a valid pointer to an initialized yaml_parser_t. See the PARSER
			// POINTER NOTE in Chunker::new.
			let event = match unsafe { Event::from_parser(self.parser) } {
				Ok(event) => event,
				Err(err) => {
					// SAFETY: The dereference of self.read_state is unsafe; see
					// the READ STATE POINTER NOTE in Chunker::new.
					return Some(Err(unsafe { (*self.read_state).error.take() }
						.unwrap_or_else(|| io::Error::new(io::ErrorKind::InvalidData, err))));
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
					// SAFETY: The dereference of self.read_state is unsafe; see
					// the READ STATE POINTER NOTE in Chunker::new.
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
					// SAFETY: The dereference of self.read_state is unsafe; see
					// the READ STATE POINTER NOTE in Chunker::new.
					let chunk = unsafe {
						(*self.read_state)
							.reader
							.take_to_offset(event.end_mark.index)
					};
					self.last_document = Some(Document {
						content: String::from_utf8(chunk).unwrap(),
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
		// SAFETY: The call to yaml_parser_delete is unsafe; see the LIBYAML
		// SAFETY NOTE and PARSER POINTER NOTE. We do not make any other calls
		// to yaml_parser_delete in this module, so we do not expect any double
		// frees.
		unsafe { yaml_parser_delete(self.parser) };

		// SAFETY: The calls to Box::from_raw are unsafe. We obtained these
		// pointers using Box::into_raw, so we expect them to satisfy all
		// requirements for conversion back into boxes and subsequent
		// deallocation. We do not call Box::from_raw with these pointers at any
		// other place in the module, so we do not expect any double frees.
		// Because we deinitialized the parser above, we do not expect it to
		// retain any references to the read state that could become invalid.
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
pub(super) enum DocumentKind {
	Scalar,
	Collection,
}

impl Document {
	/// Returns the original text of the document.
	pub(super) fn content(&self) -> &str {
		&self.content
	}

	/// Returns true if the content of the document is a scalar rather than a
	/// collection (sequence or mapping).
	pub(super) fn is_scalar(&self) -> bool {
		matches!(self.kind, DocumentKind::Scalar)
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
	unsafe fn from_parser(parser: *mut yaml_parser_t) -> Result<Event, ParserError> {
		let mut event = Box::new(MaybeUninit::<yaml_event_t>::uninit());

		// SAFETY: The call to yaml_parser_parse is unsafe; see the LIBYAML
		// SAFETY NOTE. Requirements for the validity of the parser pointer are
		// lifted to our caller, while the event pointer is expected to be valid
		// by virtue of the MaybeUninit being within a live Box.
		if unsafe { yaml_parser_parse(parser, event.as_mut_ptr()) }.fail {
			// SAFETY: The call to from_parser is unsafe as the parser pointer
			// must be a valid pointer to an initialized yaml_parser_t. We lift
			// this requirement to our caller.
			return Err(unsafe { ParserError::from_parser(parser) });
		}

		// This is effectively a hidden MaybeUninit::assume_init, like when
		// Chunker::new initializes the parser. See that comment for details.
		Ok(Event(Box::into_raw(event).cast::<yaml_event_t>()))
	}
}

impl Deref for Event {
	type Target = yaml_event_t;

	fn deref(&self) -> &Self::Target {
		// SAFETY: The conversion of the raw pointer self.0 to a shared
		// reference is unsafe. To analyze its safety, we consider the validity
		// of the pointer and the validity of the returned lifetime,
		// particularly with respect to possible aliasing.
		//
		// With respect to pointer validity, Rust defines references as pointers
		// that are aligned, not null, and point to memory containing a valid
		// value for the type. The aligned and not null conditions are satisfied
		// by obtaining the pointer through a Box (using Box::into_raw). The
		// initialization condition is satisfied by the successful call to
		// yaml_parser_parse that is required to successfully construct self.
		//
		// With respect to the lifetime, the returned reference will have the
		// same lifetime as &self. We do not destroy or deallocate the
		// referenced data outside of Drop, so we do not expect any opportunity
		// for the reference to become invalid while it is live. We do not
		// reborrow self.0 as &mut anywhere, so we do not introduce our own
		// opportunities for aliasing & and &mut. We may introduce aliasing &
		// references, but this is expected to be fine as we have &self.
		unsafe { &*self.0 }
	}
}

impl Drop for Event {
	fn drop(&mut self) {
		// SAFETY: The call to yaml_event_delete is unsafe; see the LIBYAML
		// SAFETY NOTE. We do not make any other calls to yaml_event_delete in
		// this module, so we do not expect any double frees.
		unsafe { yaml_event_delete(self.0) };

		// SAFETY: The call to Box::from_raw is unsafe. We obtained this pointer
		// using Box::into_raw, so we expect it to satisfy all requirements for
		// conversion back into a Box and subsequent deallocation. We do not
		// call Box::from_raw with this pointer at any other place in the
		// module, so we do not expect any double frees.
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
		// SAFETY: This is a relatively large unsafe block compared to the rest
		// of the module, however it really only contains two types of unsafe
		// operations: the dereferences of the parser pointer, and the calls to
		// LocatedError::from_parts.
		//
		// With respect to the parser pointer, we lift its validity requirements
		// to our caller.
		//
		// With respect to the from_parts calls, the unsafety comes from the
		// requirements on the C string pointer used to construct the error
		// description: it must be non-null and dereferenceable for the length
		// of the string, and the string must have a null terminator at the end.
		// We largely defer to the LIBYAML SAFETY NOTE, however we explicitly
		// validate that the pointers are non-null (this isn't just a check on
		// libyaml, it's possible that at least one of these will legitimately
		// be empty under normal operation).
		unsafe {
			Self {
				problem: (!(*parser).problem.is_null()).then(|| {
					LocatedError::from_parts(
						(*parser).problem.cast::<c_char>(),
						(*parser).problem_mark,
						Some((*parser).problem_offset),
					)
				}),
				context: (!(*parser).context.is_null()).then(|| {
					LocatedError::from_parts(
						(*parser).context.cast::<c_char>(),
						(*parser).context_mark,
						None,
					)
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
				None => write!(f, "{problem}"),
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
	/// `description` must be a valid pointer to a valid C-style string.
	/// Specifically: the pointer must be non-null, the string must have a null
	/// terminator at the end, and the pointer must be dereferenceable for the
	/// length of the string.
	unsafe fn from_parts(
		description: *const c_char,
		mark: yaml_mark_t,
		override_offset: Option<u64>,
	) -> Self {
		Self {
			// SAFETY: CStr::from_ptr is defined to be unsafe. We lift its
			// requirements to our caller, except for the one about the returned
			// lifetime (as we copy the CStr to an owned String during this
			// call).
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
		// While the read documentation recommends against reading from buf, it
		// does not prevent it, and does require callers of read to assume we
		// might do this. As consolation, note that we only read back bytes that
		// we know were freshly written, unless of course the source is broken
		// and lies about how many bytes it read.
		let len = self.reader.read(buf)?;
		self.captured.extend_from_slice(&buf[..len]);
		Ok(len)
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn chunker_normal_usage() {
		const INPUT: &'static str = r#"---
test: true
---
12345
---
[list, of strings]
"#;

		let chunker = Chunker::new(INPUT.as_bytes());
		let docs = chunker.collect::<Result<Vec<_>, io::Error>>().unwrap();

		let contents = docs.iter().map(|doc| doc.content()).collect::<Vec<_>>();
		assert_eq!(
			&contents,
			&[
				"---\ntest: true\n",
				"---\n12345\n",
				"---\n[list, of strings]\n",
			]
		);

		let scalars = docs.iter().map(|doc| doc.is_scalar()).collect::<Vec<_>>();
		assert_eq!(&scalars, &[false, true, false]);
	}

	#[test]
	#[should_panic]
	fn chunker_misbehaving_reader() {
		let chunker = Chunker::new(MisbehavingReader("---\nevil: true".as_bytes()));
		let _ = chunker.collect::<Vec<_>>();
	}

	/// A reader that always reports having read 1 byte more than the length of
	/// the buffer provided to [`read`](Read::read), regardless of the actual
	/// size of the underlying read.
	struct MisbehavingReader<R: Read>(R);

	impl<R: Read> Read for MisbehavingReader<R> {
		fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
			self.0.read(buf).and(Ok(buf.len() + 1))
		}
	}
}
