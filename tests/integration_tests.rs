use paste::paste;

use xt::{Format, InputHandle};

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

/// Tests that xt produces equivalent translations regardless of whether the
/// input format is auto-detected or explicitly provided.
macro_rules! xt_test_detected_vs_explicit {
  ($name:ident, $input:expr, $from:expr, $to:expr, $expected:expr) => {
    paste! {
      xt_single_test!([<$name _detected>], $input, None, $to, $expected);
      xt_single_test!([<$name _explicit>], $input, Some($from), $to, $expected);
    }
  };
}

/// Tests that xt produces equivalent translations for buffer and reader inputs.
macro_rules! xt_test_reader_vs_buffer {
  ($name:ident, $input:expr, $from:expr, $to:expr, $expected:expr) => {
    paste! {
      xt_test_detected_vs_explicit!([<$name _buffer>], InputHandle::from_buffer($input), $from, $to, $expected);
      xt_test_detected_vs_explicit!([<$name _reader>], InputHandle::from_reader($input), $from, $to, $expected);
    }
  };
}

/// Tests that all possible xt invocations for the document on the left side
/// produce the document on the right side, and vice versa.
macro_rules! xt_test_reflection {
  ($name:ident, $lf:ident, $lin:expr, $rf:ident, $rin:expr) => {
    paste! {
      xt_test_reader_vs_buffer!([<$name _ $lf:lower _to_ $rf:lower>], $lin, Format::$lf, Format::$rf, $rin);
      xt_test_reader_vs_buffer!([<$name _ $rf:lower _to_ $lf:lower>], $rin, Format::$rf, Format::$lf, $lin);
    }
  };
  ($name:ident, $lf:ident, $lin:expr) => {
    paste! {
      xt_test_reader_vs_buffer!([<$name _ $lf:lower _to_ $lf:lower>], $lin, Format::$lf, Format::$lf, $lin);
    }
  };
}

/// Exhaustively tests all possible translations between a set of documents.
macro_rules! xt_test_all_combinations {
  // A: Select the first document as our left side, or recurse and select the
  //    next document as the left side.
  ($name:ident; ($lf:ident, $lin:expr); $($rest:tt)*) => {
    xt_test_all_combinations!($name; $lf, $lin; $($rest)*); // => B or C
    xt_test_all_combinations!($name; $($rest)*);            // => A or D
  };
  // B: We have a left side and a right side. Test that combination, then test
  //    the left side against the remaining right sides.
  ($name:ident; $lf:ident, $lin:expr; ($rf:ident, $rin:expr); $($rest:tt)*) => {
    xt_test_reflection!($name, $lf, $lin, $rf, $rin);
    xt_test_all_combinations!($name; $lf, $lin; $($rest)*); // => B or C
  };
  // C: We have a left side, but we ran out of right sides. Test the left side
  //    against itself.
  ($name:ident; $lf:ident, $lin:expr;) => {
    xt_test_reflection!($name, $lf, $lin);
  };
  // D: We ran out of possible left sides.
  ($name:ident;) => {};
}

static SINGLE_JSON_INPUT: &[u8] = include_bytes!("single.json");
static SINGLE_YAML_INPUT: &[u8] = include_bytes!("single.yaml");
static SINGLE_TOML_INPUT: &[u8] = include_bytes!("single.toml");
static SINGLE_MSGPACK_INPUT: &[u8] = include_bytes!("single.msgpack");

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

xt_test_all_combinations! {
  multi;
  (Json, MULTI_JSON_INPUT);
  (Yaml, MULTI_YAML_INPUT);
  (Msgpack, MULTI_MSGPACK_INPUT);
}

#[test]
fn test_toml_reordering() {
  const INPUT: &[u8] = include_bytes!("single_reordered.json");
  const EXPECTED: &str = include_str!("single.toml");
  let mut output = Vec::with_capacity(EXPECTED.len());
  xt::translate(
    InputHandle::from_buffer(INPUT),
    Some(Format::Json),
    Format::Toml,
    &mut output,
  )
  .unwrap();
  assert_eq!(std::str::from_utf8(&output), Ok(EXPECTED));
}

const YAML_REENCODING_INPUTS: [&[u8]; 7] = [
  include_bytes!("utf16be.yaml"),
  include_bytes!("utf16le.yaml"),
  include_bytes!("utf32be.yaml"),
  include_bytes!("utf32le.yaml"),
  include_bytes!("utf8bom.yaml"),
  include_bytes!("utf16bebom.yaml"),
  include_bytes!("utf32lebom.yaml"),
];

#[test]
fn test_yaml_reencoding() {
  const EXPECTED: &str = concat!(r#"{"xt":"🧑‍💻"}"#, "\n");
  for input in YAML_REENCODING_INPUTS {
    let mut output = Vec::with_capacity(EXPECTED.len());
    xt::translate(
      InputHandle::from_buffer(input),
      Some(Format::Yaml),
      Format::Json,
      &mut output,
    )
    .unwrap();
    assert_eq!(std::str::from_utf8(&output), Ok(EXPECTED));
  }
}

#[test]
fn test_halting_yaml_deserializer_without_panic() {
  // Regression test to ensure that halting transcoding in the middle of a value
  // doesn't panic and crash. The particular example is a YAML input with a null
  // map key trying to transcode to JSON, where keys must be strings. If we're
  // not careful, we can break invariants of the YAML deserializer.
  const INPUT: &[u8] = include_bytes!("nullkey.yaml");
  let _ = xt::translate(
    InputHandle::from_buffer(INPUT),
    Some(Format::Yaml),
    Format::Json,
    std::io::sink(),
  );
}

#[test]
fn test_msgpack_depth_limit() {
  // Ensure that msgpack depth limits behave consistently for both buffer and
  // reader inputs. Buffer inputs implement their own depth check on top of
  // rmp_serde's when determining the size of the value, which should not bound
  // the limit any lower than rmp_serde itself.

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
      InputHandle::from_reader(&input[..]),
      Some(Format::Msgpack),
      Format::Msgpack,
      std::io::sink(),
    )
    .expect("reader should have been translated")
  });

  stacker::grow(STACK_SIZE, || {
    xt::translate(
      InputHandle::from_buffer(&input[..]),
      Some(Format::Msgpack),
      Format::Msgpack,
      std::io::sink(),
    )
    .expect("buffer should have been translated")
  });
}
