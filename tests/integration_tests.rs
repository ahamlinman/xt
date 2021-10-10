use jyt::{jyt, Format, InputHandle};

/// A single jyt test input.
///
/// The inputs for a given set of integration tests contain the same serialized
/// content, as formatted by jyt itself. Translating any input to any format
/// (including the source format itself) should produce the test input for that
/// format, regardless of whether the format is auto detected or specified
/// explicitly. This may impose limitations on the structure and values that the
/// test input can contain.
type TestInput = (Format, &'static [u8]);

/// Test inputs for single document transcoding.
///
/// TOML's limitations impose several restrictions on these inputs. First, no
/// input can contain a null value. Second, the root of each input must be a
/// map. Third, the values in the map must appear in an order that TOML can
/// support (non-tables appear before tables at a given level of nesting).
const SINGLE_INPUTS: [TestInput; 4] = [
  (Format::Json, include_bytes!("single.json")),
  (Format::Yaml, include_bytes!("single.yaml")),
  (Format::Toml, include_bytes!("single.toml")),
  (Format::Msgpack, include_bytes!("single.msgpack")),
];

#[test]
fn test_single_document_buffer() {
  for ((from, input), (to, expected)) in all_input_combinations(&SINGLE_INPUTS) {
    for from in [None, Some(from)] {
      let mut output = Vec::new();
      jyt(InputHandle::from_buffer(input), from, to, &mut output).unwrap();
      assert_eq!(&output, expected);
    }
  }
}

#[test]
fn test_single_document_reader() {
  for ((from, input), (to, expected)) in all_input_combinations(&SINGLE_INPUTS) {
    for from in [None, Some(from)] {
      let mut input = input;
      let mut output = Vec::new();
      jyt(InputHandle::from_reader(&mut input), from, to, &mut output).unwrap();
      assert_eq!(&output, expected);
    }
  }
}

/// Test inputs for multi document transcoding.
///
/// The current auto detection logic for MessagePack imposes a restriction on
/// these inputs: the root of the first input in the stream must be a map or
/// array. Subsequent values may be of any supported type.
const MULTI_INPUTS: [TestInput; 3] = [
  (Format::Json, include_bytes!("multi.json")),
  (Format::Yaml, include_bytes!("multi.yaml")),
  (Format::Msgpack, include_bytes!("multi.msgpack")),
];

#[test]
fn test_multi_document_buffer() {
  for ((from, input), (to, expected)) in all_input_combinations(&MULTI_INPUTS) {
    for from in [None, Some(from)] {
      let mut output = Vec::new();
      jyt(InputHandle::from_buffer(input), from, to, &mut output).unwrap();
      assert_eq!(&output, expected);
    }
  }
}

#[test]
fn test_multi_document_reader() {
  for ((from, input), (to, expected)) in all_input_combinations(&MULTI_INPUTS) {
    for from in [None, Some(from)] {
      let mut input = input;
      let mut output = Vec::new();
      jyt(InputHandle::from_reader(&mut input), from, to, &mut output).unwrap();
      assert_eq!(&output, expected);
    }
  }
}

fn all_input_combinations(inputs: &[TestInput]) -> Vec<(TestInput, TestInput)> {
  let mut result = Vec::new();
  for x in inputs {
    for y in inputs {
      result.push((*x, *y))
    }
  }
  result
}

#[test]
fn test_toml_reordering() {
  const INPUT: &'static [u8] = include_bytes!("single_reordered.json");
  const EXPECTED: &'static str = include_str!("single.toml");
  let mut output = Vec::new();
  jyt(
    InputHandle::from_buffer(INPUT),
    Some(Format::Json),
    Format::Toml,
    &mut output,
  )
  .unwrap();
  assert_eq!(std::str::from_utf8(&output), Ok(EXPECTED));
}

const YAML_REENCODING_INPUTS: [&'static [u8]; 7] = [
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
  const EXPECTED: &'static str = concat!(r#"{"jyt":"🧑‍💻"}"#, "\n");
  for input in YAML_REENCODING_INPUTS {
    let mut output = Vec::new();
    jyt(
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
fn test_yaml_halted_deserialization() {
  // Regression test to ensure that halting transcoding in the middle of a value
  // doesn't panic and crash. The particular example is a YAML input with a null
  // map key trying to transcode to JSON, where keys must be strings. If we're
  // not careful, we can break invariants of the YAML deserializer.
  const INPUT: &'static [u8] = include_bytes!("nullkey.yaml");
  let mut output = Vec::new();
  let _ = jyt(
    InputHandle::from_buffer(INPUT),
    Some(Format::Yaml),
    Format::Json,
    &mut output,
  );
}