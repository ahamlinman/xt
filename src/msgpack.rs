//! The MessagePack data format.

use std::error;
use std::fmt::{self, Display};
use std::io::{self, BufRead, BufReader, Read, Write};

use rmp::Marker;
use rmp_serde::decode::Error::{InvalidDataRead, InvalidMarkerRead};
use serde::Deserialize;

use crate::input::{self, Input, Ref};
use crate::transcode;

/// The maximum allowed nesting depth of MessagePack values.
///
/// This particular value is the undocumented default from rmp_serde, which
/// seems to be enough to reliably prevent stack overflows on debug builds of
/// the program using the default main thread stack size on Linux and macOS.
const DEPTH_LIMIT: usize = 1024;

pub(crate) fn input_matches(mut input: Ref) -> io::Result<bool> {
	// In MessagePack, any byte below 0x80 represents a literal unsigned
	// integer. That means any ASCII text input is a valid multi-document
	// MessagePack stream, where every "document" is practically meaningless. To
	// prevent these kinds of weird matches, we only detect input as MessagePack
	// when the first byte indicates that the next value will be a map or array.
	// Arbitrary non-ASCII input that happens to match one of these markers
	// (e.g. certain UTF-8 multibyte sequences) is extremely unlikely to be a
	// valid sequence of MessagePack values.
	if !matches!(
		input.prefix(1)?.first().copied().map(Marker::from_u8),
		Some(
			Marker::FixArray(_)
				| Marker::Array16
				| Marker::Array32
				| Marker::FixMap(_)
				| Marker::Map16 | Marker::Map32
		)
	) {
		return Ok(false);
	}

	let result = match &mut input {
		Ref::Slice(b) => match_input_buffer(b),
		Ref::Reader(r) => match_input_reader(r),
	};
	match result {
		Err(InvalidMarkerRead(err) | InvalidDataRead(err)) => Err(err),
		Err(_) => Ok(false),
		Ok(()) => Ok(true),
	}
}

fn match_input_buffer(input: &[u8]) -> Result<(), rmp_serde::decode::Error> {
	let mut de = rmp_serde::Deserializer::from_read_ref(input);
	de.set_max_depth(DEPTH_LIMIT);
	serde::de::IgnoredAny::deserialize(&mut de).and(Ok(()))
}

fn match_input_reader<R: Read>(input: R) -> Result<(), rmp_serde::decode::Error> {
	let mut de = rmp_serde::Deserializer::new(input);
	de.set_max_depth(DEPTH_LIMIT);
	serde::de::IgnoredAny::deserialize(&mut de).and(Ok(()))
}

pub(crate) fn transcode<O>(input: input::Handle, mut output: O) -> crate::Result<()>
where
	O: crate::Output,
{
	match input.into() {
		Input::Slice(b) => {
			let mut rest = &*b;
			while !rest.is_empty() {
				let next;
				(next, rest) = rest.split_at(next_value_size(rest, DEPTH_LIMIT)?);
				let mut de = rmp_serde::Deserializer::from_read_ref(next);
				de.set_max_depth(DEPTH_LIMIT);
				output.transcode_from(&mut de)?;
			}
		}
		Input::Reader(r) => {
			let mut r = BufReader::new(r);
			while !r.fill_buf()?.is_empty() {
				let mut de = rmp_serde::Deserializer::new(&mut r);
				de.set_max_depth(DEPTH_LIMIT);
				output.transcode_from(&mut de)?;
			}
		}
	}
	Ok(())
}

pub(crate) struct Output<W: Write>(W);

impl<W: Write> Output<W> {
	pub(crate) fn new(w: W) -> Output<W> {
		Output(w)
	}
}

impl<W: Write> crate::Output for Output<W> {
	fn transcode_from<'de, D, E>(&mut self, de: D) -> crate::Result<()>
	where
		D: serde::de::Deserializer<'de, Error = E>,
		E: serde::de::Error + 'static,
	{
		let mut ser = rmp_serde::Serializer::new(&mut self.0);
		transcode::transcode(&mut ser, de)?;
		Ok(())
	}

	fn transcode_value<S>(&mut self, value: S) -> crate::Result<()>
	where
		S: serde::ser::Serialize,
	{
		let mut ser = rmp_serde::Serializer::new(&mut self.0);
		value.serialize(&mut ser)?;
		Ok(())
	}

	fn flush(&mut self) -> io::Result<()> {
		self.0.flush()
	}
}

/// Returns the size in bytes of the MessagePack value at the start of the input
/// slice.
///
/// Data after the MessagePack value at the start of the input is ignored. The
/// size of an empty input slice is 0.
///
/// This function guarantees that the input can be sliced to the returned size
/// without panicking, even if the input is not well-formed. For example, a
/// MessagePack str or bin value with a reported length larger than the
/// remainder of the input slice will produce an error.
///
/// TODO: A [`ReadRefReader`][rmp_serde::decode::ReadRefReader] could directly
/// tell us how much of its input slice is remaining if we could access it from
/// a `Deserializer`. That would remove the need for this custom logic.
fn next_value_size(input: &[u8], depth_limit: usize) -> Result<usize, ReadSizeError> {
	if depth_limit == 0 {
		return Err(ReadSizeError::DepthLimitExceeded);
	}
	if input.is_empty() {
		return Ok(0);
	}

	let marker = rmp::Marker::from_u8(input[0]);
	let total_size = match marker {
		Marker::Reserved => return Err(ReadSizeError::InvalidMarker),

		Marker::Null | Marker::True | Marker::False | Marker::FixPos(_) | Marker::FixNeg(_) => 1,

		Marker::U8 | Marker::I8 => 2,
		Marker::U16 | Marker::I16 => 3,
		Marker::U32 | Marker::I32 | Marker::F32 => 5,
		Marker::U64 | Marker::I64 | Marker::F64 => 9,

		Marker::FixExt1 => 3,
		Marker::FixExt2 => 4,
		Marker::FixExt4 => 6,
		Marker::FixExt8 => 10,
		Marker::FixExt16 => 18,
		Marker::Ext8 => 3 + try_read_length_8(input)? as usize,
		Marker::Ext16 => 4 + try_read_length_16(input)? as usize,
		Marker::Ext32 => 6 + try_read_length_32(input)? as usize,

		Marker::FixStr(n) => 1 + n as usize,
		Marker::Str8 | Marker::Bin8 => 2 + try_read_length_8(input)? as usize,
		Marker::Str16 | Marker::Bin16 => 3 + try_read_length_16(input)? as usize,
		Marker::Str32 | Marker::Bin32 => 5 + try_read_length_32(input)? as usize,

		Marker::FixArray(count) => 1 + total_seq_size(&input[1..], count, depth_limit)?,
		Marker::FixMap(pairs) => 1 + total_map_size(&input[1..], pairs, depth_limit)?,
		Marker::Array16 => {
			let count = try_read_length_16(input)?;
			3 + total_seq_size(&input[3..], count, depth_limit)?
		}
		Marker::Map16 => {
			let pairs = try_read_length_16(input)?;
			3 + total_map_size(&input[3..], pairs, depth_limit)?
		}
		Marker::Array32 => {
			let count = try_read_length_32(input)?;
			5 + total_seq_size(&input[5..], count, depth_limit)?
		}
		Marker::Map32 => {
			let pairs = try_read_length_32(input)?;
			5 + total_map_size(&input[5..], pairs, depth_limit)?
		}
	};

	if total_size <= input.len() {
		Ok(total_size)
	} else {
		Err(ReadSizeError::Truncated)
	}
}

fn total_seq_size<N>(input: &[u8], count: N, depth_limit: usize) -> Result<usize, ReadSizeError>
where
	N: Into<u32>,
{
	let count = count.into();
	let mut total = 0;
	let mut seq = input;
	for _ in 0..count {
		if seq.is_empty() {
			return Err(ReadSizeError::Truncated);
		}
		let size = next_value_size(seq, depth_limit - 1)?;
		total += size;
		seq = &seq[size..];
	}
	Ok(total)
}

fn total_map_size<N>(input: &[u8], pairs: N, depth_limit: usize) -> Result<usize, ReadSizeError>
where
	N: Into<u32>,
{
	let pairs = pairs.into();
	let first = total_seq_size(input, pairs, depth_limit)?;
	let second = total_seq_size(&input[first..], pairs, depth_limit)?;
	Ok(first + second)
}

fn try_read_length_8(input: &[u8]) -> Result<u8, ReadSizeError> {
	try_read_length(input, u8::from_be_bytes)
}

fn try_read_length_16(input: &[u8]) -> Result<u16, ReadSizeError> {
	try_read_length(input, u16::from_be_bytes)
}

fn try_read_length_32(input: &[u8]) -> Result<u32, ReadSizeError> {
	try_read_length(input, u32::from_be_bytes)
}

fn try_read_length<const N: usize, T, F>(input: &[u8], convert: F) -> Result<T, ReadSizeError>
where
	F: FnOnce([u8; N]) -> T,
{
	Ok(convert(
		input
			.get(1..1 + N)
			.ok_or(ReadSizeError::Truncated)?
			.try_into()
			.unwrap(),
	))
}

/// The error type returned by [`next_value_size`].
#[derive(Clone, Debug, Eq, PartialEq)]
enum ReadSizeError {
	/// A MessagePack value in the input was truncated.
	Truncated,
	/// A MessagePack value in the input used the reserved marker byte `0xc1`.
	InvalidMarker,
	/// The maximum allowed nesting depth of MessagePack values was exceeded.
	DepthLimitExceeded,
}

impl error::Error for ReadSizeError {}

impl Display for ReadSizeError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			ReadSizeError::Truncated => f.write_str("unexpected end of MessagePack input"),
			ReadSizeError::InvalidMarker => f.write_str("invalid MessagePack marker in input"),
			ReadSizeError::DepthLimitExceeded => f.write_str("depth limit exceeded"), // same message as rmp_serde
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	use hex_literal::hex;

	const VALID_INPUTS: &[&[u8]] = &[
		// empty
		&[],
		// nil
		&hex!("c0"),
		// bool (false)
		&hex!("c2"),
		// bool (true)
		&hex!("c3"),
		// positive fixint
		&hex!("2a"),
		// negative fixint
		&hex!("f4"),
		// 8-bit unsigned
		&hex!("cc 09"),
		// 16-bit unsigned
		&hex!("cd 09 f9"),
		// 32-bit unsigned
		&hex!("ce 09 f9 11 02"),
		// 64-bit unsigned
		&hex!("cf 09 f9 11 02 9d 74 e3 5b"),
		// 8-bit signed
		&hex!("d0 d8"),
		// 16-bit signed
		&hex!("d1 d8 41"),
		// 32-bit signed
		&hex!("d2 d8 41 56 c5"),
		// 64-bit signed
		&hex!("d3 d8 41 56 c5 63 56 88 c0"),
		// single precision
		&hex!("ca 64 7a 5a 6e"),
		// double precision
		&hex!("cb 54 79 4b 50 45 67 4e 64"),
		// fixstr ("xt")
		&hex!("a2 78 74"),
		// str 8 ("xt")
		&hex!("d9 02 78 74"),
		// str 16 ("xt")
		&hex!("da 00 02 78 74"),
		// str 32 ("xt")
		&hex!("db 00 00 00 02 78 74"),
		// fixstr ("")
		&hex!("a0"),
		// str 8 ("")
		&hex!("d9 00"),
		// str 16 ("")
		&hex!("da 00 00"),
		// str 32 ("")
		&hex!("db 00 00 00 00"),
		// bin 8 (b"xt")
		&hex!("c4 02 78 74"),
		// bin 16 (b"xt")
		&hex!("c5 00 02 78 74"),
		// bin 32 (b"xt")
		&hex!("c6 00 00 00 02 78 74"),
		// bin 8 (b"")
		&hex!("c4 00"),
		// bin 16 (b"")
		&hex!("c5 00 00"),
		// bin 32 (b"")
		&hex!("c6 00 00 00 00"),
		// fixarray (["xt", true])
		&hex!("92 a2 78 74 c3"),
		// array 16 (["xt", true])
		&hex!("dc 00 02 a2 78 74 c3"),
		// array 32 (["xt", true])
		&hex!("dd 00 00 00 02 a2 78 74 c3"),
		// fixarray ([])
		&hex!("90"),
		// array 16 ([])
		&hex!("dc 00 00"),
		// array 32 ([])
		&hex!("dd 00 00 00 00"),
		// fixmap ({"xt": true, "good": true})
		&hex!("82 a2 78 74 c3 a4 67 6f 6f 64 c3"),
		// map 16 ({"xt": true, "good": true})
		&hex!("de 00 02 a2 78 74 c3 a4 67 6f 6f 64 c3"),
		// map 32 ({"xt": true, "good": true})
		&hex!("df 00 00 00 02 a2 78 74 c3 a4 67 6f 6f 64 c3"),
		// fixmap ({})
		&hex!("80"),
		// map 16 ({})
		&hex!("de 00 00"),
		// map 32 ({})
		&hex!("df 00 00 00 00"),
		// fixext 1
		&hex!("d4 01 09"),
		// fixext 2
		&hex!("d5 01 09 f9"),
		// fixext 4
		&hex!("d6 01 09 f9 11 02"),
		// fixext 8
		&hex!("d7 01 09 f9 11 02 9d 74 e3 5b"),
		// fixext 16
		&hex!("d8 01 09 f9 11 02 9d 74 e3 5b d8 41 56 c5 63 56 88 c0"),
		// ext 8
		&hex!("c7 04 01 09 f9 11 02"),
		// ext 16
		&hex!("c8 00 04 01 09 f9 11 02"),
		// ext 32
		&hex!("c9 00 00 00 04 01 09 f9 11 02"),
	];

	#[test]
	fn valid_input_size() {
		for input in VALID_INPUTS {
			assert_eq!(next_value_size(input, DEPTH_LIMIT), Ok(input.len()));
		}
	}

	#[test]
	fn truncated_valid_input_size() {
		for input in VALID_INPUTS.iter().filter(|i| i.len() > 1) {
			for len in 1..(input.len() - 1) {
				assert_eq!(
					next_value_size(&input[..len], DEPTH_LIMIT),
					Err(ReadSizeError::Truncated)
				);
			}
		}
	}

	#[test]
	fn nonsensically_large_input_size() {
		// The string "xt," but with a reported length of 2^32-1 bytes.
		assert_eq!(
			next_value_size(&hex!("db ff ff ff ff 78 74"), DEPTH_LIMIT),
			Err(ReadSizeError::Truncated)
		);
	}

	#[test]
	fn excessively_deep_input_size() {
		// [[true]]
		assert_eq!(next_value_size(&hex!("91 91 c3"), 3), Ok(3));
		// [[[true]]]
		assert_eq!(
			next_value_size(&hex!("91 91 91 c3"), 3),
			Err(ReadSizeError::DepthLimitExceeded)
		);
	}

	#[test]
	fn invalid_marker_size() {
		// <invalid>
		assert_eq!(
			next_value_size(&hex!("c1"), DEPTH_LIMIT),
			Err(ReadSizeError::InvalidMarker)
		);
		// ["xt", <invalid>]
		assert_eq!(
			next_value_size(&hex!("92 a2 78 74 c1"), DEPTH_LIMIT),
			Err(ReadSizeError::InvalidMarker)
		);
		// {"xt": true, "good": <invalid>}
		assert_eq!(
			next_value_size(&hex!("82 a2 78 74 c3 a4 67 6f 6f 64 c1"), DEPTH_LIMIT),
			Err(ReadSizeError::InvalidMarker)
		);
	}

	#[test]
	fn size_skipping_invalid_suffixes() {
		// true; <invalid>
		assert_eq!(next_value_size(&hex!("c3 c1"), DEPTH_LIMIT), Ok(1));
		// ["xt"]; <invalid>
		assert_eq!(next_value_size(&hex!("91 a2 78 74 c1"), DEPTH_LIMIT), Ok(4));
	}
}
