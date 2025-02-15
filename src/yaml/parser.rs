//! A second try at the YAML chunker, focusing on more granular abstractions
//! around unsafe code and limiting panics in case I ever need real libyaml in
//! the future.

use std::error::Error;
use std::ffi::{c_char, c_void, CStr};
use std::fmt::Display;
use std::io::{self, Read};
use std::mem::MaybeUninit;
use std::ops::{Deref, DerefMut};
use std::ptr;

use unsafe_libyaml::{
	yaml_encoding_t::YAML_UTF8_ENCODING, yaml_event_delete, yaml_event_t, yaml_mark_t,
	yaml_parser_delete, yaml_parser_initialize, yaml_parser_parse, yaml_parser_set_encoding,
	yaml_parser_set_input, yaml_parser_t,
};

pub(super) use unsafe_libyaml::{
	YAML_DOCUMENT_END_EVENT, YAML_DOCUMENT_START_EVENT, YAML_MAPPING_START_EVENT,
	YAML_SCALAR_EVENT, YAML_SEQUENCE_START_EVENT, YAML_STREAM_END_EVENT,
};
pub(super) struct Parser<R: Read> {
	parser: *mut yaml_parser_t,
	read_state: *mut ReadState<R>,
}

struct ReadState<R: Read> {
	reader: R,
	buffer: Vec<u8>,
	error: Option<io::Error>,
}

impl<R: Read> Parser<R> {
	pub(super) fn new(reader: R) -> Parser<R> {
		let mut parser = Box::new(MaybeUninit::<yaml_parser_t>::uninit());
		let read_state = Box::into_raw(Box::new(ReadState {
			reader,
			buffer: vec![],
			error: None,
		}));
		unsafe {
			if yaml_parser_initialize(parser.as_mut_ptr()).fail {
				panic!("out of memory for libyaml parser initialization");
			};
			yaml_parser_set_encoding(parser.as_mut_ptr(), YAML_UTF8_ENCODING);
			yaml_parser_set_input(
				parser.as_mut_ptr(),
				Self::read_handler,
				read_state.cast::<c_void>(),
			);
		};
		Parser {
			parser: Box::into_raw(parser).cast::<yaml_parser_t>(),
			read_state,
		}
	}

	unsafe fn read_handler(
		read_state: *mut c_void,
		buffer: *mut u8,
		buffer_size: u64,
		size_read: *mut u64,
	) -> i32 {
		const READ_SUCCESS: i32 = 1;
		const READ_FAILURE: i32 = 0;

		if read_state.is_null() || buffer.is_null() || size_read.is_null() {
			return READ_FAILURE;
		}

		let read_state = unsafe { &mut *read_state.cast::<ReadState<R>>() };
		let buffer_size = usize::try_from(buffer_size).unwrap();
		read_state.buffer.resize(buffer_size, 0);

		match read_state.reader.read(&mut read_state.buffer[..]) {
			Ok(read_len) if read_len <= buffer_size => {
				unsafe {
					ptr::copy_nonoverlapping(read_state.buffer.as_ptr(), buffer, read_len);
					*size_read = read_len as u64;
				}
				read_state.error = None;
				READ_SUCCESS
			}
			Err(err) => {
				read_state.error = Some(err);
				READ_FAILURE
			}
			_ => {
				read_state.error = Some(io::Error::new(io::ErrorKind::Other, "misbehaving reader"));
				READ_FAILURE
			}
		}
	}
}

impl<R: Read> Drop for Parser<R> {
	fn drop(&mut self) {
		unsafe {
			yaml_parser_delete(self.parser);
			drop(Box::from_raw(self.parser));
			drop(Box::from_raw(self.read_state));
		}
	}
}

impl<R: Read> Deref for Parser<R> {
	type Target = yaml_parser_t;

	fn deref(&self) -> &Self::Target {
		unsafe { &*self.parser }
	}
}

impl<R: Read> DerefMut for Parser<R> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		unsafe { &mut *self.parser }
	}
}

impl<R: Read> Parser<R> {
	pub(super) fn reader_mut(&mut self) -> &mut R {
		unsafe { &mut (*self.read_state).reader }
	}

	pub(super) fn parse(&mut self) -> Result<Event, io::Error> {
		let result = unsafe { Event::new(self) };
		match result {
			Ok(result) => Ok(result),
			Err(_) => Err(unsafe { (*self.read_state).error.take() }
				.unwrap_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "TODO"))),
		}
	}
}

pub(super) struct Event(*mut yaml_event_t);

impl Event {
	unsafe fn new(parser: &mut yaml_parser_t) -> Result<Event, ()> {
		let mut event = Box::new(MaybeUninit::<yaml_event_t>::uninit());
		unsafe {
			if yaml_parser_parse(parser, event.as_mut_ptr()).fail {
				return Err(());
			}
		}
		Ok(Event(Box::into_raw(event).cast::<yaml_event_t>()))
	}
}

impl Deref for Event {
	type Target = yaml_event_t;

	fn deref(&self) -> &Self::Target {
		unsafe { &*self.0 }
	}
}

impl Drop for Event {
	fn drop(&mut self) {
		unsafe {
			yaml_event_delete(self.0);
			drop(Box::from_raw(self.0));
		};
	}
}

#[derive(Debug)]
struct ParserError {
	problem: Option<LocatedError>,
	context: Option<LocatedError>,
}

impl ParserError {
	unsafe fn new(parser: &mut yaml_parser_t) -> Self {
		unsafe {
			Self {
				problem: (!parser.problem.is_null()).then(|| {
					LocatedError::from_parts(
						parser.problem.cast::<c_char>(),
						parser.problem_mark,
						Some(parser.problem_offset),
					)
				}),
				context: (!parser.context.is_null()).then(|| {
					LocatedError::from_parts(
						parser.context.cast::<c_char>(),
						parser.context_mark,
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

#[derive(Debug)]
struct LocatedError {
	description: String,
	offset: u64,
	line: u64,
	column: u64,
}

impl LocatedError {
	unsafe fn from_parts(
		description: *const c_char,
		mark: yaml_mark_t,
		override_offset: Option<u64>,
	) -> Self {
		Self {
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
