//! A second try at the YAML chunker, focusing on more granular abstractions
//! around unsafe code and limiting panics in case I ever need real libyaml in
//! the future.

use std::ffi::c_void;
use std::io::{self, Read};
use std::mem::MaybeUninit;
use std::ptr;

use unsafe_libyaml::{
	yaml_encoding_t::YAML_UTF8_ENCODING, yaml_parser_delete, yaml_parser_initialize,
	yaml_parser_set_encoding, yaml_parser_set_input, yaml_parser_t,
};

struct Parser<R: Read> {
	parser: *mut yaml_parser_t,
	read_state: *mut ReadState<R>,
}

struct ReadState<R: Read> {
	reader: R,
	buffer: Vec<u8>,
	error: Option<io::Error>,
}

impl<R: Read> Parser<R> {
	fn new(reader: R) -> Parser<R> {
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
