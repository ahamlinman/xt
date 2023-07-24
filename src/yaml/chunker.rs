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
		// TODO: The docs for Box<T> talk about how it asserts uniqueness over
		// its contents (LLVM noalias), and that using raw pointers derived from
		// a Box after mutating through it "is not allowed." Miri has never
		// complained about this usage, but it is quite possible that this usage
		// is disallowed and could cause problems in the future. This requires
		// further analysis; serde_yaml defines a special "Owned" pointer type
		// that might be a useful reference.
		let mut parser = Box::new(MaybeUninit::<yaml_parser_t>::uninit());

		// SAFETY: The call to yaml_parser_initialize is unsafe as it is an
		// FFI-like call to libyaml. From our side, we provide a valid (aligned
		// and non-null due to its derivation from a Box) pointer to an
		// allocated (but uninitialized) yaml_parser_t; see above for details.
		// We expect that libyaml is not so broken that it reads potentially
		// uninitialized memory during initialization, and cursory manual
		// inspection of the libyaml code seems to indicate that is the case.
		// Otherwise, we assume that libyaml is implemented correctly and
		// satisfies all required safety properties.
		if unsafe { yaml_parser_initialize(parser.as_mut_ptr()) }.fail {
			panic!("out of memory for libyaml parser initialization");
		}

		// NOTE: The cast here is from `*mut MaybeUninit<yaml_parser_t>` to
		// `*mut yaml_parser_t`. This is effectively a hidden assume_init, but
		// is expected to be safe as MaybeUninit<T> is guaranteed to have the
		// same layout as T, and because the yaml_parser_t was initialized above
		// by yaml_parser_initialize.
		let parser = Box::into_raw(parser).cast::<yaml_parser_t>();

		// SAFETY: The call to yaml_parser_set_encoding is unsafe as it is an
		// FFI-like call to libyaml. From our side, we provide a valid pointer
		// to an allocated and initialized yaml_parser_t; see above for details.
		// No special invariants are expected in relation to the second
		// parameter that specifies the text encoding for the input. Otherwise,
		// we assume that libyaml is implemented correctly and satisfies all
		// required safety properties.
		unsafe { yaml_parser_set_encoding(parser, YAML_UTF8_ENCODING) };

		// See below for details.
		let read_state = Box::into_raw(Box::new(ReadState {
			reader: ChunkReader::new(reader),
			buffer: vec![],
			error: None,
		}));

		// SAFETY: The call to yaml_parser_set_input is unsafe as it is an
		// FFI-like call to libyaml. From our side, there are two important
		// aspects to consider when analyzing the safety of this call.
		//
		// First, we provide a valid pointer to an allocated and initialized
		// yaml_parser_t; see above for details.
		//
		// Second, our use of Self::read_handler imposes an additional safety
		// requirement as noted in the documentation for that function: we must
		// ensure that the data pointer passed to yaml_parser_set_input along
		// with the instantiation of that function is a valid pointer to an
		// initialized ReadState<R>. The validity of the pointer (properly
		// allocated, aligned, non-null) is guaranteed through its derivation
		// from a Box, while the proper initialization of the ReadState<R> value
		// is guaranteed by the Rust compiler (as the type does not impose any
		// special initialization requirements that Rust cannot safely
		// represent).
		//
		// Otherwise, we assume that libyaml is implemented properly and
		// satisfies all required safety properties.
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
	/// ReadState<R>.
	unsafe fn read_handler(
		read_state: *mut c_void,
		buffer: *mut u8,
		size: u64,
		size_read: *mut u64,
	) -> i32 {
		const READ_SUCCESS: i32 = 1;
		const READ_FAILURE: i32 = 0;

		// NOTE: To limit the potential for any unintended aliasing issues that
		// could result from the use of a &mut reference, all uses of
		// `read_state` in this function are done through the raw *mut pointer.
		// These dereferences are expected to be safe due to the safety
		// requirements imposed on callers with respect to the read handler's
		// data pointer, as explained in the function documentation.
		// Furthermore, all reads and writes to fields within the ReadState<R>
		// are expected to be safe as we require the caller to properly
		// initialize this value.
		let read_state = read_state.cast::<ReadState<R>>();

		// This represents the size of an in-memory buffer, which cannot
		// possibly exceed usize::MAX unless libyaml is seriously broken.
		#[allow(clippy::cast_possible_truncation)]
		let size = size as usize;

		// SAFETY: The dereference of read_state is the only unsafe operation;
		// see the NOTE on read_state.
		unsafe { (*read_state).buffer.resize(size, 0) };

		// SAFETY: The dereference of read_state is the only unsafe operation;
		// see the NOTE on read_state.
		match unsafe { (*read_state).reader.read(&mut (*read_state).buffer[..]) } {
			Ok(len) => {
				// This check is critical for soundness; see the `Read::read`
				// documentation for details.
				if len > size {
					// SAFETY: The dereference of read_state is the only unsafe
					// operation; see the NOTE on read_state.
					unsafe {
						(*read_state).error =
							Some(io::Error::new(io::ErrorKind::Other, "misbehaving reader"));
					}
					// TODO: We generally treat the unsafe_libyaml code as if we
					// were doing FFI to libyaml in C, in which case it would be
					// unsafe to panic and potentially unwind here. However,
					// it's actually a C to Rust transpilation, which should
					// mean that unwinding panics are safe, and possibly a
					// better way to handle this serious of an error, right?
					return READ_FAILURE;
				}

				// SAFETY: There are two separate unsafe operations to consider
				// in this block: the dereference and write through read_state,
				// and the call to ptr::copy_nonoverlapping.
				//
				// For an analysis of the safety of the read_state usage, see
				// the NOTE on the definition of read_state itself.
				//
				// To analyze the safety of the call to ptr::copy_nonoverlapping,
				// we will consider its four documented safety requirements in
				// turn.
				//
				// First: the pointer returned by `(*read_state).buffer.as_ptr()`
				// must be valid for reads of `len` bytes. We assume that the
				// call to the safe as_ptr function in the Rust standard library
				// will return a valid pointer. With respect to the size of the
				// read, the pointed-to buffer was resized to `size` bytes
				// through a call to Vec::resize above, and as such we expect
				// that the pointer will be valid for reads of up to `size`
				// bytes. Because we return above when `len > size`, we can
				// guarantee at the point of this call that `len <= size`, and
				// by extension that a pointer valid for reads of `size` bytes
				// will also be valid for reads of `len` bytes.
				//
				// Second: the `buffer` pointer must be valid for writes of
				// `len` bytes. This pointer is provided by libyaml along with
				// the corresponding `size` value, which we expect to represent
				// the size of the pointed-to buffer, i.e. the maximum number of
				// bytes for which it is valid to write through the buffer
				// pointer. In terms of the validity of the pointer, we largely
				// assume that libyaml behaves correctly, however we do confirm
				// that the pointer is non-null. In terms of the size of the
				// write, we once again note that `len <= size`, and that a
				// pointer valid for writes of `size` bytes should therefore be
				// valid for writes of `len` bytes.
				//
				// Third: Both the source and destination pointers must be
				// properly aligned. Both are pointers to u8, a type whose size
				// is guaranteed by definition to be 1 byte (assuming 8-bit
				// bytes, as I am not aware of Rust supporting platforms using
				// other byte sizes). Because "the size of a value is always a
				// multiple of its alignment" (per the "Type Layout" section of
				// the Rust Reference), 1 must be a (presumably integer)
				// multiple of the alignment of a u8, which means that the
				// alignment of a u8 must itself be 1. As such, any non-null u8
				// pointer must be aligned.
				//
				// Fourth: The region of memory beginning at the source pointer
				// with a size of `len` bytes must not overlap with the region
				// of memory beginning at the destination pointer with the same
				// size. We expect the global allocator to satisfy this property
				// on our behalf, given that the buffer provided by libyaml and
				// the buffer that we manage within read_state are distinct
				// allocations.
				//
				// TODO: Inspect the libyaml code to fully prove that the buffer
				// pointer meets all of the validity requirements that are
				// documented in std::ptr but not explicitly checked here.
				unsafe {
					if !buffer.is_null() {
						ptr::copy_nonoverlapping((*read_state).buffer.as_ptr(), buffer, len);
					}
				}

				// SAFETY: size_read is a raw pointer provided by libyaml. We
				// largely assume that libyaml is implemented such that the
				// pointer it provides is valid for the write of a single u64,
				// however we do confirm that the pointer is non-null (similar
				// to our handling of the buffer pointer above). We do not
				// assume that the pointed-to memory is initialized or that the
				// pointer is aligned, even though it actually might be (and
				// likely is). As such, we assume that it is safe to write a u64
				// through this pointer using ptr::write_unaligned, but not
				// necessarily using normal assignment or ptr::write. Note that
				// as u64 is Copy, it cannot implement Drop, and therefore we do
				// not expect any kind of leak due to the use of write_unaligned
				// rather than a normal assignment.
				//
				// TODO: Consider inspecting the libyaml code to better
				// understand the guarantees around this pointer. In particular:
				// can we guarantee that it is (and always will be, even in
				// future versions of libyaml) safe to write through this
				// pointer using normal assignment, or even just ptr::write?
				unsafe {
					if !size_read.is_null() {
						ptr::write_unaligned(size_read, len as u64);
					}
				}

				// Note that EOF does not require special handling, as libyaml's
				// EOF condition is the same as Rust's: report a successful read
				// of 0 bytes.

				// SAFETY: The dereference of read_state is the only unsafe
				// operation; see the NOTE on read_state.
				unsafe { (*read_state).error = None };
				READ_SUCCESS
			}
			Err(err) => {
				// SAFETY: The dereference of read_state is the only unsafe
				// operation; see the NOTE on read_state.
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
			// SAFETY: The call to Event::from_parser is unsafe as it requires a
			// valid pointer to an initialized yaml_parser_t. Chunker::new is
			// expected to handle this for self.parser; see the SAFETY comments
			// in that function for details.
			let event = match unsafe { Event::from_parser(self.parser) } {
				Ok(event) => event,
				Err(err) => {
					// SAFETY: The dereference of self.read_state is unsafe.
					// Chunker::new is expected to initialize both this pointer
					// and the underlying ReadState<R> properly; see the SAFETY
					// comments in that function for details.
					//
					// TODO: Prove that there is no aliasing between the &mut
					// taken to the read_state's error here and the &mut taken
					// to the read_state's error in the read handler.
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
					// SAFETY: The dereference of self.read_state is unsafe. See
					// above for discussion around this.
					//
					// TODO: Prove that there is no aliasing between the &mut
					// taken to the reader here and the &mut taken to the reader
					// in the read handler.
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
					// SAFETY: There are two separate unsafe operations to
					// consider in this block: the dereference of self.read_state,
					// and the call to String::from_utf8_unchecked.
					//
					// See above for analysis of the read_state dereference.
					//
					// For the from_utf8_unchecked call, we expect libyaml to
					// validate that the input is UTF-8 during parsing, due to
					// us calling yaml_parser_set_encoding with YAML_UTF8_ENCODING
					// while initializing the parser in Chunker::new. A cursory
					// inspection of the libyaml code does not reveal any obvious
					// issues with its handling of UTF-8 encoding errors.
					//
					// TODO: Consider inspecting the libyaml code more deeply to
					// fully prove that it handles invalid UTF-8 correctly. Even
					// then, it might make sense to just do a checked UTF-8
					// conversion as a hedge against potential future libyaml
					// bugs (as unlikely as that sounds), given how fast UTF-8
					// encoding checks typically are.
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
		// SAFETY: The call to yaml_parser_delete is unsafe as it is an FFI-like
		// call to libyaml. From our side, we pass a valid pointer to an
		// initialized yaml_parser_t created in Chunker::new; see the SAFETY
		// comments in that function for details. We do not make any other calls
		// to yaml_parser_delete anywhere in this module, thus we are not at
		// risk of double-freeing any resources associated with the parser.
		// Otherwise, we assume that libyaml is implemented correctly and
		// satisfies all required safety properties.
		unsafe { yaml_parser_delete(self.parser) };

		// SAFETY: The calls to Box::from_raw are unsafe. Both of these raw
		// pointers were originally obtained using Box::into_raw in
		// Chunker::new, which means that they are expected to satisfy all of
		// the general safety requirements required to convert them back into
		// boxes. We do not call Box::from_raw with these pointers at any other
		// place in the module, nor do we expose these raw pointers outside of
		// the struct, so we do not expect any risk of a double-free.
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
		// TODO: This pattern is similar to how Chunker::new initializes a
		// yaml_parser_t, and raises the same questions.
		let mut event = Box::new(MaybeUninit::<yaml_event_t>::uninit());

		// SAFETY: The call to yaml_parser_parse is unsafe as it is an FFI-like
		// call to libyaml. From our side, we provide a valid parser pointer
		// (due to safety requirements documented for our caller) and a valid
		// event pointer (to data allocated in a Box, which is expected to meet
		// Rust's pointer validity requirements). Otherwise, we assume that
		// libyaml is implemented correctly and satisfies all required safety
		// properties.
		if unsafe { yaml_parser_parse(parser, event.as_mut_ptr()) }.fail {
			// SAFETY: The call to from_parser is unsafe as the parser pointer
			// must be a valid pointer to an initialized yaml_parser_t. We
			// document this requirement for our caller.
			return Err(unsafe { ParserError::from_parser(parser) });
		}

		// NOTE: This cast from `*mut MaybeUninit<yaml_event_t>` to `*mut
		// yaml_event_t` is basically a hidden MaybeUninit::assume_init. This is
		// expected to be safe as MaybeUninit<T> is guaranteed to have the same
		// size, alignment, and ABI as T (and, of course, because we expect
		// yaml_parser_parse to initialize the pointed-to memory).
		Ok(Event(Box::into_raw(event).cast::<yaml_event_t>()))
	}
}

impl Deref for Event {
	type Target = yaml_event_t;

	fn deref(&self) -> &Self::Target {
		// SAFETY: The conversion of the raw pointer self.0 to a shared
		// reference is unsafe. To analyze the safety of this, we consider the
		// validity of the pointer and the validity of the returned lifetime,
		// particularly with respect to possible aliasing.
		//
		// With respect to validity of the pointer: Rust defines references as
		// pointers that are aligned, not null, and point to memory containing a
		// valid value for the type. The aligned and not null conditions are
		// satisfied by obtaining the pointer through a Box (using Box::into_raw).
		// The initialization condition is satisfied by the successful call to
		// yaml_parser_parse that is required to successfully construct self.
		//
		// With respect to the lifetime: The returned reference will have the
		// same lifetime as &self. We do not destroy or deallocate the
		// referenced data outside of Drop, so we do not expect any opportunity
		// for the reference to become invalid while it is live. We do not
		// reborrow self.0 as &mut anywhere else in the type, so we do not
		// introduce our own opportunities for aliasing & and &mut. We do not
		// release the raw self.0 outside of this type, so we do not provide a
		// clear means for external callers to violate these conditions.
		unsafe { &*self.0 }
	}
}

impl Drop for Event {
	fn drop(&mut self) {
		// SAFETY: The call to yaml_event_delete is unsafe as it is an FFI-like
		// call to libyaml. From our side, we pass a valid pointer to a
		// yaml_event_t that was initialized when self was constructed. We do
		// not make any other calls to this function anywhere else in the
		// module, so we do not expect any risk of a double-free. Otherwise, we
		// assume that libyaml is implemented correctly and satisfies all
		// required safety properties.
		unsafe { yaml_event_delete(self.0) };

		// SAFETY: The call to Box::from_raw is unsafe. This raw pointer was
		// originally obtained using Box::into_raw when self was constructed,
		// which means that it is expected to satisfy all requirements for
		// conversion back into a Box. We do not call Box::from_raw with this
		// pointer anywhere else in the module, nor do we expose the raw pointer
		// outside of this type, so we do not expect any risk of a double-free.
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
		// We expect all dereferences of parser to be safe due to requirements
		// documented for our callers.
		//
		// With respect to the from_parts calls, the unsafety comes from the
		// requirements on the C string pointer used to construct the error
		// description. We explicitly validate that each possible description
		// pointer is non-null before we attempt to construct a LocatedError
		// with that description. Otherwise, we assume that libyaml is
		// implemented correctly, such that when these pointers are non-null the
		// associated strings have proper null terminators and are each
		// contained within a single allocated object.
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
	/// `description` must be a valid pointer to a valid C-style string.
	/// Specifically: the pointer must be non-null, the string must have a null
	/// terminator at the end, and the pointer must be valid for reads up to and
	/// including the terminator (i.e. the entire memory range must be contained
	/// within a single allocated object).
	unsafe fn from_parts(
		description: *const c_char,
		mark: yaml_mark_t,
		override_offset: Option<u64>,
	) -> Self {
		Self {
			// SAFETY: CStr::from_ptr is defined to be unsafe. Its safety
			// requirements are lifted directly into the safety requirements for
			// our callers, with the exception of the note about the returned
			// lifetime (as we copy the CStr into an owned String during this
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
