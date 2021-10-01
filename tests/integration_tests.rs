use jyt::{jyt, Format, InputHandle};

type TestInput = (Format, &'static [u8]);

const SINGLE_INPUTS: [TestInput; 4] = [
  (Format::Json, include_bytes!("single.json")),
  (Format::Yaml, include_bytes!("single.yaml")),
  (Format::Toml, include_bytes!("single.toml")),
  (Format::Msgpack, include_bytes!("single.msgpack")),
];

#[test]
fn test_single_document_buffer_known_format() {
  for ((from, input), (to, expected)) in all_input_combinations(&SINGLE_INPUTS) {
    let input = InputHandle::from_buffer(input);
    let mut output = Vec::new();
    jyt(input, Some(from), to, &mut output).unwrap();
    assert_eq!(&output, expected);
  }
}

#[test]
fn test_single_document_reader_known_format() {
  for ((from, input), (to, expected)) in all_input_combinations(&SINGLE_INPUTS) {
    let mut input = input;
    let input = InputHandle::from_reader(&mut input);
    let mut output = Vec::new();
    jyt(input, Some(from), to, &mut output).unwrap();
    assert_eq!(&output, expected);
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
