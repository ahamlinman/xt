//! xt's integration test suite.
//!
//! Most of xt's integration tests are based on the philosophy that xt should
//! produce consistent output for a given input regardless of how it consumes
//! that input. That is, xt should always work the same way whether it reads
//! from a file or a stream, or whether it auto-detects the input format or
//! knows it in advance.
//!
//! The test suite looks at sets of documents containing the same serialized
//! content as translated and output by xt itself, and exhaustively checks all
//! possible xt invocations—yes, all O(n²) of them—for translating one of those
//! documents to another (including itself). Besides generating a quadratic
//! blow-up of test cases, this approach imposes limitations on the structure
//! and values of the test inputs within a given set, and can cause some
//! annoyance when the specific formatting of a given output changes. However,
//! it does provide broad coverage with relatively little effort.

use std::io;
use std::str::from_utf8;
use std::thread;

use rstest::rstest;

use xt::Format;

macro_rules! xt_assert_translation {
	(
		input_source = $input_source:path;
		translator = $translator:path;
		translation = $from:expr => $to:expr;
		source_format = $source_format:expr;
	) => {
		let input = $input_source($from);
		let expected = $input_source($to);
		let mut output = Vec::with_capacity(expected.len());
		$translator(input, $source_format, $to, &mut output).unwrap();

		if let (Ok(expected), Ok(output)) = (from_utf8(expected), from_utf8(&output)) {
			// Try to print out readable representations of these values if we
			// can, instead of just arrays of bytes...
			similar_asserts::assert_eq!(expected, output);
		} else {
			// ...but always make sure we at least print *something*.
			similar_asserts::assert_eq!(expected, output);
		}
	};
}

#[rstest]
fn translate_single_slice_detected(
	#[values(Format::Json, Format::Yaml, Format::Toml, Format::Msgpack)] from: Format,
	#[values(Format::Json, Format::Yaml, Format::Toml, Format::Msgpack)] to: Format,
) {
	xt_assert_translation! {
		input_source = get_single_document_input;
		translator = xt::translate_slice;
		translation = from => to;
		source_format = None;
	}
}

#[rstest]
fn translate_single_slice_explicit(
	#[values(Format::Json, Format::Yaml, Format::Toml, Format::Msgpack)] from: Format,
	#[values(Format::Json, Format::Yaml, Format::Toml, Format::Msgpack)] to: Format,
) {
	xt_assert_translation! {
		input_source = get_single_document_input;
		translator = xt::translate_slice;
		translation = from => to;
		source_format = Some(from);
	}
}

#[rstest]
fn translate_single_reader_detected(
	#[values(Format::Json, Format::Yaml, Format::Toml, Format::Msgpack)] from: Format,
	#[values(Format::Json, Format::Yaml, Format::Toml, Format::Msgpack)] to: Format,
) {
	xt_assert_translation! {
		input_source = get_single_document_input;
		translator = xt::translate_reader;
		translation = from => to;
		source_format = None;
	}
}

#[rstest]
fn translate_single_reader_explicit(
	#[values(Format::Json, Format::Yaml, Format::Toml, Format::Msgpack)] from: Format,
	#[values(Format::Json, Format::Yaml, Format::Toml, Format::Msgpack)] to: Format,
) {
	xt_assert_translation! {
		input_source = get_single_document_input;
		translator = xt::translate_reader;
		translation = from => to;
		source_format = Some(from);
	}
}

#[rstest]
fn translate_multi_slice_detected(
	#[values(Format::Json, Format::Yaml, Format::Msgpack)] from: Format,
	#[values(Format::Json, Format::Yaml, Format::Msgpack)] to: Format,
) {
	xt_assert_translation! {
		input_source = get_multi_document_input;
		translator = xt::translate_slice;
		translation = from => to;
		source_format = None;
	}
}

#[rstest]
fn translate_multi_slice_explicit(
	#[values(Format::Json, Format::Yaml, Format::Msgpack)] from: Format,
	#[values(Format::Json, Format::Yaml, Format::Msgpack)] to: Format,
) {
	xt_assert_translation! {
		input_source = get_multi_document_input;
		translator = xt::translate_slice;
		translation = from => to;
		source_format = Some(from);
	}
}

#[rstest]
fn translate_multi_reader_detected(
	#[values(Format::Json, Format::Yaml, Format::Msgpack)] from: Format,
	#[values(Format::Json, Format::Yaml, Format::Msgpack)] to: Format,
) {
	xt_assert_translation! {
		input_source = get_multi_document_input;
		translator = xt::translate_reader;
		translation = from => to;
		source_format = None;
	}
}

#[rstest]
fn translate_multi_reader_explicit(
	#[values(Format::Json, Format::Yaml, Format::Msgpack)] from: Format,
	#[values(Format::Json, Format::Yaml, Format::Msgpack)] to: Format,
) {
	xt_assert_translation! {
		input_source = get_multi_document_input;
		translator = xt::translate_reader;
		translation = from => to;
		source_format = Some(from);
	}
}

/// Returns the single-document test input for a given format.
///
/// TOML's limitations impose several restrictions on these inputs:
///
/// 1. No null values.
/// 2. The root of each input must be a map.
/// 3. The values of the map must be ordered such that all non-map values appear
///    before any maps at a given depth.
fn get_single_document_input(fmt: Format) -> &'static [u8] {
	match fmt {
		Format::Json => include_bytes!("single.json"),
		Format::Yaml => include_bytes!("single.yaml"),
		Format::Toml => include_bytes!("single.toml"),
		Format::Msgpack => include_bytes!("single.msgpack"),
		fmt => panic!("{fmt} does not have a single-document test case"),
	}
}

/// Returns the multi-document test input for a given format.
///
/// The YAML and MessagePack format detection logic imposes a restriction on
/// these inputs: the first input in the stream must be a map or sequence.
/// Subsequent values may be of any supported type.
///
/// TOML does not support multi-document transcoding.
fn get_multi_document_input(fmt: Format) -> &'static [u8] {
	match fmt {
		Format::Json => include_bytes!("multi.json"),
		Format::Yaml => include_bytes!("multi.yaml"),
		Format::Msgpack => include_bytes!("multi.msgpack"),
		fmt => panic!("{fmt} does not have a multi-document test case"),
	}
}

/// Tests the translation of YAML documents from various text encodings.
///
/// YAML 1.2 requires support for the UTF-8, UTF-16, and UTF-32 character
/// encodings. Because serde_yaml only supports UTF-8 as of this writing, xt
/// takes care of re-encoding inputs where necessary. The test inputs cover a
/// reasonable subset of combinations of code unit size, endianness, and
/// presence or lack of a BOM.
#[rstest]
fn yaml_encoding(
	#[values("utf16be", "utf16le", "utf32be", "utf32le", "utf16bebom", "utf32lebom")] name: &str,
) {
	let input = get_yaml_encoding_input(name);
	let mut output = Vec::with_capacity(YAML_ENCODING_RESULT.len());
	xt::translate_slice(input, Some(Format::Yaml), Format::Json, &mut output).unwrap();
	assert_eq!(std::str::from_utf8(&output), Ok(YAML_ENCODING_RESULT));
}

const YAML_ENCODING_RESULT: &str = concat!(r#"{"xt":"🧑‍💻"}"#, "\n");

fn get_yaml_encoding_input(name: &str) -> &'static [u8] {
	match name {
		"utf16be" => include_bytes!("utf16be.yaml"),
		"utf16le" => include_bytes!("utf16le.yaml"),
		"utf32be" => include_bytes!("utf32be.yaml"),
		"utf32le" => include_bytes!("utf32le.yaml"),
		"utf16bebom" => include_bytes!("utf16bebom.yaml"),
		"utf32lebom" => include_bytes!("utf32lebom.yaml"),
		name => panic!("{name} is not a known YAML encoding input"),
	}
}

/// Tests that TOML output re-orders inputs as needed to meet TOML-specific
/// requirements, in particular that all non-table values must appear before any
/// tables at the same level.
#[test]
fn toml_reordering() {
	const INPUT: &[u8] = include_bytes!("single_reordered.json");
	const EXPECTED: &str = include_str!("single.toml");
	let mut output = Vec::with_capacity(EXPECTED.len());
	xt::translate_slice(INPUT, Some(Format::Json), Format::Toml, &mut output).unwrap();
	assert_eq!(std::str::from_utf8(&output), Ok(EXPECTED));
}

/// Tests that a TOML input that starts with a table is not accidentally
/// mis-detected as YAML. This happened with an early version of streaming YAML
/// input support, since a YAML parser can successfully parse a TOML table
/// header as a valid document containing a flow sequence, and not actually fail
/// until later in the stream.
#[test]
fn toml_initial_table_detection() {
	const INPUT: &[u8] = include_bytes!("initial_table.toml");
	xt::translate_reader(INPUT, None, Format::Json, io::sink()).unwrap();
}

/// Tests that halting transcoding in the middle of a YAML input does not panic
/// and crash.
///
/// The particular example involves translating a YAML input with a null key to
/// JSON, which refuses to accept the non-string key. Past versions of xt's
/// transcoder broke internal YAML deserializer variants when this happened.
#[test]
fn yaml_halting_without_panic() {
	const INPUT: &[u8] = include_bytes!("nullkey.yaml");
	let _ = xt::translate_slice(INPUT, Some(Format::Yaml), Format::Json, std::io::sink());
}

/// Tests that MessagePack recursion depth limits behave consistently for both
/// buffer and reader inputs.
///
/// Buffer inputs implement their own depth check on top of rmp_serde's when
/// they determine the sizes of input values. It should behave consistently with
/// rmp_serde, which performs the only depth check for reader inputs.
///
/// See https://stackoverflow.com/a/42960702 for context around the additional
/// thread used in this test. Cargo runs tests on secondary threads, which by
/// default have 2 MiB stacks (per std::thread docs as of writing). This is
/// apparently too small to properly test the normal depth limit, so we run
/// these cases with stacks that better approximate a typical main thread.
#[test]
fn msgpack_depth_limit() {
	// Magic private constant from msgpack.rs.
	const DEPTH_LIMIT: usize = 1024;

	// Nested arrays enclosing a null.
	let mut input = [0x91_u8; DEPTH_LIMIT];
	*input.last_mut().unwrap() = 0xc0;

	let builder = thread::Builder::new().stack_size(8 * 1024 * 1024);
	builder
		.spawn(move || {
			xt::translate_reader(
				&input[..],
				Some(Format::Msgpack),
				Format::Msgpack,
				std::io::sink(),
			)
			.expect("reader should have been translated");

			xt::translate_slice(
				&input[..],
				Some(Format::Msgpack),
				Format::Msgpack,
				std::io::sink(),
			)
			.expect("buffer should have been translated");
		})
		.unwrap()
		.join()
		.unwrap();
}
