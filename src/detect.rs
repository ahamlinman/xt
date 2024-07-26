//! Automatic detection of data formats based on parser trials.

use std::io;

use crate::{input, Format};

/// Detects the input format by trying each known format and selecting the first
/// one that works.
pub(crate) fn detect_format(input: &mut input::Handle) -> io::Result<Option<Format>> {
	// As a binary format, we expect MessagePack to be more restrictive than any
	// text format. Detection of MessagePack inputs is limited to collection
	// types; see comments in the implementation for details.
	if crate::msgpack::input_matches(input.borrow_mut())? {
		return Ok(Some(Format::Msgpack));
	}

	// We expect JSON to be more restrictive than other text formats. For
	// example, a "#" comment at the start of a document could be TOML or YAML,
	// but definitely not JSON.
	if crate::json::input_matches(input.borrow_mut())? {
		return Ok(Some(Format::Json));
	}

	// YAML is actually less restrictive than TOML, but we want to try it first
	// since it supports streaming input (which means that detection may require
	// less buffering). Detection of YAML inputs is limited to collection types;
	// see comments in the implementation for details.
	if crate::yaml::input_matches(input.borrow_mut())? {
		return Ok(Some(Format::Yaml));
	}

	// Finally, TOML is the only input format that must fully buffer input
	// before parsing.
	if crate::toml::input_matches(input.borrow_mut())? {
		return Ok(Some(Format::Toml));
	}

	Ok(None)
}
