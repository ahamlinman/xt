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
//! blow-up of test cases, this may impose limitations on the structure and
//! values of the test inputs within a given set, and may cause some annoyance
//! if the specific formatting of a given output ever changes. However, I'm not
//! sure of another approach that would cover this much with this little effort.
//!
//! "Little," that is, if you're ready for some fun with macros.

use std::io;

use paste::paste;

use xt::{Format, Handle};

/// Tests a single call to xt::translate against expected output.
macro_rules! xt_single_test {
	($name:ident, $input:expr, $from:expr, $to:expr, $expected:expr) => {
		#[test]
		fn $name() {
			let mut output = Vec::with_capacity($expected.len());
			xt::translate($input, $from, $to, &mut output).unwrap();
			assert_eq!(&output, $expected);
		}
	};
}

/// Tests that xt produces equivalent translations for all possible invocations
/// that translate a given input to a given output format.
macro_rules! xt_test_all_invocations {
	($name:ident, $input:expr, $from:expr, $to:expr, $expected:expr) => {
		paste! {
			xt_single_test!([<$name _buffer_detected>], Handle::from_slice($input), None, $to, $expected);
			xt_single_test!([<$name _reader_detected>], Handle::from_reader($input), None, $to, $expected);
			xt_single_test!([<$name _buffer_explicit>], Handle::from_slice($input), Some($from), $to, $expected);
			xt_single_test!([<$name _reader_explicit>], Handle::from_reader($input), Some($from), $to, $expected);
		}
	}
}

/// Exhaustively tests all possible translations between a set of documents.
macro_rules! xt_test_all_combinations {
	// A: Select the first document as our left side and test it against the
	//    remaining right sides, or recurse and select the next document as the
	//    left side.
	{ $name:ident; ($lf:ident, $lin:expr); $($tail:tt)* } => {
		xt_test_all_combinations!{ $name; $lf, $lin; $($tail)* } // => B or C
		xt_test_all_combinations!{ $name; $($tail)* }            // => A or D
	};
	// B: We have a left side and a right side. Test both possible translation
	//    directions for that pair, then test the left side against the remaining
	//    right sides.
	{ $name:ident; $lf:ident, $lin:expr; ($rf:ident, $rin:expr); $($tail:tt)* } => {
		paste! {
			xt_test_all_invocations!([<$name _ $lf:lower _to_ $rf:lower>], $lin, Format::$lf, Format::$rf, $rin);
			xt_test_all_invocations!([<$name _ $rf:lower _to_ $lf:lower>], $rin, Format::$rf, Format::$lf, $lin);
		}
		xt_test_all_combinations!{ $name; $lf, $lin; $($tail)* } // => B or C
	};
	// C: We have a left side, but we ran out of right sides. Test the left side
	//    against itself.
	{ $name:ident; $lf:ident, $lin:expr; } => {
		paste! {
			xt_test_all_invocations!([<$name _ $lf:lower _to_ $lf:lower>], $lin, Format::$lf, Format::$lf, $lin);
		}
	};
	// D: We ran out of possible left sides.
	{ $name:ident; } => {};
}

static SINGLE_JSON_INPUT: &[u8] = include_bytes!("single.json");
static SINGLE_YAML_INPUT: &[u8] = include_bytes!("single.yaml");
static SINGLE_TOML_INPUT: &[u8] = include_bytes!("single.toml");
static SINGLE_MSGPACK_INPUT: &[u8] = include_bytes!("single.msgpack");

// Tests single document transcoding.
//
// TOML's limitations impose several restrictions on these inputs:
//
// 1. No null values.
// 2. The root of each input must be a map.
// 3. The values in the map must appear in an order that TOML can support
//    (non-tables before tables at a given level of nesting).
xt_test_all_combinations! {
	single;
	(Json, SINGLE_JSON_INPUT);
	(Yaml, SINGLE_YAML_INPUT);
	(Toml, SINGLE_TOML_INPUT);
	(Msgpack, SINGLE_MSGPACK_INPUT);
}

static MULTI_JSON_INPUT: &[u8] = include_bytes!("multi.json");
static MULTI_YAML_INPUT: &[u8] = include_bytes!("multi.yaml");
static MULTI_MSGPACK_INPUT: &[u8] = include_bytes!("multi.msgpack");

// Tests multi-document transcoding.
//
// The current MessagePack auto detection logic imposes a restriction on these
// inputs: the root of the first input in the stream must be a map or array.
// Subsequent values may be of any supported type.
//
// TOML does not support multi-document transcoding.
xt_test_all_combinations! {
	multi;
	(Json, MULTI_JSON_INPUT);
	(Yaml, MULTI_YAML_INPUT);
	(Msgpack, MULTI_MSGPACK_INPUT);
}

const YAML_ENCODING_RESULT: &str = concat!(r#"{"xt":"🧑‍💻"}"#, "\n");

/// Tests the translation of YAML documents from various text encodings.
///
/// YAML 1.2 requires support for the UTF-8, UTF-16, and UTF-32 character
/// encodings. Because yaml-rust (and by extension serde_yaml) only supports
/// UTF-8 as of this writing, xt takes care of re-encoding inputs where
/// necessary. The test inputs cover a reasonable subset of the possible
/// combinations of code unit size, type of endianness, and presence or lack of
/// a BOM.
macro_rules! xt_test_yaml_encodings {
	($($filename:ident),+ $(,)?) => {
		paste! {
			$(#[test]
			fn [<yaml_encoding_ $filename>]() {
				static INPUT: &[u8] = include_bytes!(concat!(stringify!($filename), ".yaml"));
				let mut output = Vec::with_capacity(YAML_ENCODING_RESULT.len());
				xt::translate(
					Handle::from_slice(INPUT),
					Some(Format::Yaml),
					Format::Json,
					&mut output,
				)
				.unwrap();
				assert_eq!(std::str::from_utf8(&output), Ok(YAML_ENCODING_RESULT));
			})+
		}
	};
}

xt_test_yaml_encodings![utf16be, utf16le, utf32be, utf32le, utf16bebom, utf32lebom];

/// Tests that TOML output re-orders inputs as needed to meet TOML-specific
/// requirements, in particular that all non-table values must appear before any
/// tables at the same level.
#[test]
fn toml_reordering() {
	const INPUT: &[u8] = include_bytes!("single_reordered.json");
	const EXPECTED: &str = include_str!("single.toml");
	let mut output = Vec::with_capacity(EXPECTED.len());
	xt::translate(
		Handle::from_slice(INPUT),
		Some(Format::Json),
		Format::Toml,
		&mut output,
	)
	.unwrap();
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
	xt::translate(Handle::from_reader(INPUT), None, Format::Json, io::sink()).unwrap();
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
	let _ = xt::translate(
		Handle::from_slice(INPUT),
		Some(Format::Yaml),
		Format::Json,
		std::io::sink(),
	);
}

/// Tests that MessagePack recursion depth limits behave consistently for both
/// buffer and reader inputs.
///
/// Buffer inputs implement their own depth check on top of rmp_serde's when
/// they determine the sizes of input values. It should behave consistently with
/// rmp_serde, which performs the only depth check for reader inputs.
#[test]
fn msgpack_depth_limit() {
	// Magic private constant from msgpack.rs.
	const DEPTH_LIMIT: usize = 1024;

	// Nested arrays enclosing a null.
	let mut input = [0x91_u8; DEPTH_LIMIT];
	*input.last_mut().unwrap() = 0xc0;

	// See https://stackoverflow.com/a/42960702. Cargo runs tests on secondary
	// threads, which by default have 2 MiB stacks (per current std::thread docs).
	// This is apparently too small to properly test our normal depth limit, so we
	// run these cases with stacks that better approximate a typical main thread.
	const STACK_SIZE: usize = 8 * 1024 * 1024;

	stacker::grow(STACK_SIZE, || {
		xt::translate(
			Handle::from_reader(&input[..]),
			Some(Format::Msgpack),
			Format::Msgpack,
			std::io::sink(),
		)
		.expect("reader should have been translated")
	});

	stacker::grow(STACK_SIZE, || {
		xt::translate(
			Handle::from_slice(&input[..]),
			Some(Format::Msgpack),
			Format::Msgpack,
			std::io::sink(),
		)
		.expect("buffer should have been translated")
	});
}
