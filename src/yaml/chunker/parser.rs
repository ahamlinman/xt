//! A minimal safe abstraction over the venerable [libyaml].
//!
//! [`Parser`] reads a UTF-8 encoded YAML stream and exposes [`Event`]s that
//! indicate the start and end positions of various document features. It's
//! based on a pure-Rust translation of the common [libyaml] library, and
//! largely exposes that library's types and values when it's safe to do so.
//!
//! [libyaml]: https://pyyaml.org/wiki/LibYAML

use std::error::Error;
use std::ffi::{c_char, c_void, CStr};
use std::fmt::Display;
use std::io::{self, Read};
use std::mem::MaybeUninit;
use std::ptr;

use unsafe_libyaml::{
	yaml_encoding_t::YAML_UTF8_ENCODING, yaml_event_delete, yaml_event_t, yaml_event_type_t,
	yaml_mark_t, yaml_parser_delete, yaml_parser_initialize, yaml_parser_parse,
	yaml_parser_set_encoding, yaml_parser_set_input, yaml_parser_t,
};

pub(super) use unsafe_libyaml::yaml_event_type_t::*;

pub(super) struct Parser<R>
where
	R: Read,
{
	parser: Box<yaml_parser_t>,
	read_state: *mut ReadState<R>, // See new() for details.
}

struct ReadState<R>
where
	R: Read,
{
	reader: R,
	bouncer: Vec<u8>,
	error: Option<io::Error>,
}

impl<R> Parser<R>
where
	R: Read,
{
	pub(super) fn new(reader: R) -> Parser<R> {
		let mut parser: Box<yaml_parser_t>;

		// SAFETY: This comes from libyaml, which we assume is implemented
		// correctly. We expect initialization functions to properly handle
		// uninitialized memory; empirical tests in Miri show this to be true.
		unsafe {
			// TODO: Use Box::new_uninit and Box::assume_init if our MSRV hits
			// or exceeds 1.82.
			let mut uninit = Box::new(MaybeUninit::<yaml_parser_t>::uninit());
			if yaml_parser_initialize(uninit.as_mut_ptr()).ok {
				parser = Box::from_raw(Box::into_raw(uninit).cast());
			} else {
				panic!("out of memory for yaml_parser_initialize");
			}
		}

		// libyaml needs a raw ReadState<R> pointer for Self::read_handler.
		// Since mutable access through a Box invalidates derived pointers,
		// we can't simultaneously keep this as a Box in Parser and allow
		// reader_mut() to expose the underlying reader. reader_mut() is a
		// must-have, so we solve the aliasing concern by keeping this raw for
		// its entire life. new() should no longer panic after this point,
		// so this should never leak before we finish constructing the Parser
		// and activate its Drop impl.
		let read_state = Box::into_raw(Box::new(ReadState {
			reader,
			bouncer: vec![],
			error: None,
		}));

		// SAFETY: Again, we assume libyaml is implemented correctly. We know
		// the parser is initialized because we didn't panic above.
		unsafe {
			yaml_parser_set_encoding(&mut *parser, YAML_UTF8_ENCODING);
			yaml_parser_set_input(
				&mut *parser,
				Self::read_handler,
				read_state.cast::<c_void>(),
			);
		};

		Parser { parser, read_state }
	}

	fn read_state_mut(&mut self) -> &mut ReadState<R> {
		// SAFETY: The only other dereference of self.read_state outside of Drop
		// is in read_handler, when we pull it out of libyaml's data pointer.
		// Assuming libyaml is single-threaded and we have no other aliasing
		// bugs, our &mut self guarantees that nobody is running the parser
		// (and, by extension, read_handler). The output lifetime is bounded by
		// self, so nobody _can_ run the parser while it remains live.
		unsafe { &mut *self.read_state }
	}

	pub(super) fn reader_mut(&mut self) -> &mut R {
		&mut self.read_state_mut().reader
	}

	pub(super) fn next_event(&mut self) -> Result<Event, io::Error> {
		Event::parse_next(&mut self.parser).map_err(|err| {
			self.read_state_mut()
				.error
				.take()
				.unwrap_or_else(|| io::Error::new(io::ErrorKind::InvalidData, err))
		})
	}

	/// A callback for libyaml to read from a Rust Read impl.
	///
	/// # Safety
	///
	/// The data pointer provided to `yaml_parser_set_input` must be a valid
	/// `ReadState<R>` pointer.
	unsafe fn read_handler(
		read_state: *mut c_void,
		buffer: *mut u8,
		buffer_size: u64,
		size_read: *mut u64,
	) -> i32 {
		const READ_SUCCESS: i32 = 1;
		const READ_FAILURE: i32 = 0;

		// These should never fail, but let's be extra safe. Ideally we would
		// panic in cases as degenerate as this, but I want to model a scenario
		// where we're using the true libyaml through FFI and can't unwind.
		if read_state.is_null() || buffer.is_null() || size_read.is_null() {
			return READ_FAILURE;
		}
		let Ok(buffer_size) = usize::try_from(buffer_size) else {
			return READ_FAILURE;
		};

		// SAFETY: self.read_state_mut() is the only other dereference of the
		// read_state; see its comment explaining how it's mutually exclusive
		// with running the parser. The lifetime here lasts through the end of
		// this function, i.e. before the parser is finished.
		let read_state = unsafe { &mut *read_state.cast::<ReadState<R>>() };

		// libyaml is not guaranteed to initialize its buffer prior to the first
		// read. It would be instant Undefined Behavior to slice that buffer,
		// and unsound to expose it to a safe Read impl, so we need to bounce
		// reads through a buffer we control.
		read_state.bouncer.resize(buffer_size, 0);

		match read_state.reader.read(&mut read_state.bouncer[..]) {
			Ok(read_len) if read_len <= buffer_size => {
				// SAFETY: copy_nonoverlapping is VERY dangerous, so let's walk
				// through its 4 requirements:
				//
				// 1. read_state.bouncer.as_ptr() must be valid for reads of
				//    read_len bytes. We resize that to buffer_size above, and
				//    our match guard guarantees read_len <= buffer_size, so
				//    we're good.
				//
				// 2. buffer must be valid for writes of read_len bytes. We know
				//    read_len <= buffer_size, and otherwise trust libyaml to
				//    pass us valid arguments.
				//
				// 3. Both pointers must be aligned. They're u8 pointers, so are
				//    inherently aligned because "the size of a value is always
				//    a multiple of its alignment" (The Rust Reference § 10.3).
				//
				// 4. The memory regions can't overlap. We control the
				//    bounce buffer, so the only way libyaml's can overlap is if
				//    the global allocator is truly busted.
				//
				// As far as the *size_read write, we're again trusting libyaml
				// to pass valid arguments. Note that libyaml's EOF condition is
				// the same as Rust's: report a successful 0 byte read.
				unsafe {
					ptr::copy_nonoverlapping(read_state.bouncer.as_ptr(), buffer, read_len);
					*size_read = read_len as u64;
				}
				read_state.error = None;
				READ_SUCCESS
			}
			Ok(_) => {
				read_state.error = Some(io::Error::new(io::ErrorKind::Other, "misbehaving reader"));
				READ_FAILURE
			}
			Err(err) => {
				read_state.error = Some(err);
				READ_FAILURE
			}
		}
	}
}

impl<R> Drop for Parser<R>
where
	R: Read,
{
	fn drop(&mut self) {
		// SAFETY: Parser::new panics if libyaml fails to initialize the parser,
		// so we know it's logically valid here. self.read_state originally came
		// from a Box, so is safe to deallocate that way. We logically destroy
		// the parser before the read state, so it should have no chance to
		// access freed read state memory.
		unsafe {
			yaml_parser_delete(&mut *self.parser);
			drop(Box::from_raw(self.read_state));
		}
	}
}

pub(super) struct Event(yaml_event_t);

impl Event {
	fn parse_next(parser: &mut yaml_parser_t) -> Result<Event, ParserError> {
		let mut event = MaybeUninit::uninit();
		// SAFETY: We assume yaml_parser_parse is implemented correctly, and
		// logically initializes the event when it succeeds. If it fails, we
		// simply drop the MaybeUninit when we return the error.
		unsafe {
			if yaml_parser_parse(parser, event.as_mut_ptr()).ok {
				Ok(Event(event.assume_init()))
			} else {
				Err(ParserError::new(parser))
			}
		}
	}

	pub(super) fn event_type(&self) -> yaml_event_type_t {
		self.0.type_
	}

	pub(super) fn start_offset(&self) -> u64 {
		self.0.start_mark.index
	}

	pub(super) fn end_offset(&self) -> u64 {
		self.0.end_mark.index
	}
}

impl Drop for Event {
	fn drop(&mut self) {
		// SAFETY: Event::parse_next returns an error if libyaml fails to
		// initialize the event, so we know it's logically valid here.
		unsafe {
			yaml_event_delete(&mut self.0);
		};
	}
}

#[derive(Debug)]
struct ParserError {
	problem: Option<LocatedError>,
	context: Option<LocatedError>,
}

impl ParserError {
	fn new(parser: &mut yaml_parser_t) -> Self {
		let (problem, context);
		// SAFETY: We assume libyaml is implemented correctly with respect to
		// these being either null pointers or valid C strings. The null pointer
		// checks aren't out of caution; at least one of these may legitimately
		// be null under normal operation.
		unsafe {
			// unsafe_libyaml makes these *const i8 on every target, rather than
			// dynamically using the correct signedness like `c_char` does.
			// Without the casts working around that, builds for Linux on Arm
			// targets (and others) will fail.
			problem = Self::try_cstr_into_string(parser.problem.cast::<c_char>());
			context = Self::try_cstr_into_string(parser.context.cast::<c_char>());
		}
		Self {
			problem: problem.map(|description| {
				LocatedError::from_parts(
					description,
					parser.problem_mark,
					Some(parser.problem_offset),
				)
			}),
			context: context.map(|description| {
				LocatedError::from_parts(description, parser.context_mark, None)
			}),
		}
	}

	/// Try to convert a C string into a [`String`], lossily replacing invalid
	/// UTF-8 sequences, or return [`None`] if `ptr` is null.
	///
	/// # Safety
	///
	/// `ptr` must meet all requirements documented by [`CStr::from_ptr`].
	unsafe fn try_cstr_into_string(ptr: *const c_char) -> Option<String> {
		// SAFETY: Delegated to the caller.
		(!ptr.is_null()).then(|| unsafe { CStr::from_ptr(ptr).to_string_lossy().into_owned() })
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

#[derive(Debug)]
struct LocatedError {
	description: String,
	offset: u64,
	line: u64,
	column: u64,
}

impl LocatedError {
	fn from_parts(description: String, mark: yaml_mark_t, override_offset: Option<u64>) -> Self {
		Self {
			description,
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
