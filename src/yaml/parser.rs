//! A second try at the YAML chunker, focusing on more granular abstractions
//! around unsafe code and limiting panics in case I ever need real libyaml in
//! the future.

use std::error::Error;
use std::ffi::{c_void, CStr};
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

	fn parser_mut(&mut self) -> &mut yaml_parser_t {
		// SAFETY: There is no other direct use of self.parser outside of Drop,
		// so no way for us to mistakenly alias this. The output lifetime is
		// bounded by self on return.
		unsafe { &mut *self.parser }
	}

	fn read_state_mut(&mut self) -> &mut ReadState<R> {
		// SAFETY: The only other dereference of self.read_state outside of Drop
		// is in read_handler, when we pull it out of libyaml's data pointer.
		// Assuming libyaml is single-threaded and we have no other aliasing
		// bugs, our &mut self guarantees that nobody is running the parser
		// (and, by extension, read_handler) right now. The output lifetime is
		// bounded by self on return.
		unsafe { &mut *self.read_state }
	}

	pub(super) fn reader_mut(&mut self) -> &mut R {
		&mut self.read_state_mut().reader
	}

	pub(super) fn parse(&mut self) -> Result<Event, io::Error> {
		Event::new(self.parser_mut()).map_err(|err| {
			self.read_state_mut()
				.error
				.take()
				.unwrap_or_else(|| io::Error::new(io::ErrorKind::InvalidData, err))
		})
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

impl<R: Read> Drop for Parser<R> {
	fn drop(&mut self) {
		unsafe {
			yaml_parser_delete(self.parser);
			drop(Box::from_raw(self.parser));
			drop(Box::from_raw(self.read_state));
		}
	}
}

pub(super) struct Event(*mut yaml_event_t);

impl Event {
	fn new(parser: &mut yaml_parser_t) -> Result<Event, ParserError> {
		let mut event = Box::new(MaybeUninit::<yaml_event_t>::uninit());
		if unsafe { yaml_parser_parse(parser, event.as_mut_ptr()) }.fail {
			return Err(ParserError::new(parser));
		}
		Ok(Event(Box::into_raw(event).cast::<yaml_event_t>()))
	}

	fn event(&self) -> &yaml_event_t {
		unsafe { &*self.0 }
	}

	pub(super) fn event_type(&self) -> yaml_event_type_t {
		self.event().type_
	}

	pub(super) fn start_offset(&self) -> u64 {
		self.event().start_mark.index
	}

	pub(super) fn end_offset(&self) -> u64 {
		self.event().end_mark.index
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
	fn new(parser: &mut yaml_parser_t) -> Self {
		let (problem, context);
		unsafe {
			problem = (!parser.problem.is_null()).then(|| {
				CStr::from_ptr(parser.problem)
					.to_string_lossy()
					.into_owned()
			});
			context = (!parser.context.is_null()).then(|| {
				CStr::from_ptr(parser.context)
					.to_string_lossy()
					.into_owned()
			});
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
