use std::convert::TryInto;
use std::error::Error;
use std::fmt::{self, Display};
use std::io::{self, BufRead, BufReader, Read, Write};

use crate::{transcode, Input, InputRef};

pub(crate) fn transcode<O>(input: InputRef, mut output: O) -> Result<(), Box<dyn Error>>
where
  O: crate::Output,
{
  match input.into() {
    Input::Buffered(buf) => {
      let mut buf = buf.deref();
      while buf.len() > 0 {
        let size = next_value_size(buf)?;
        let (next, rest) = buf.split_at(size);
        let mut de = rmp_serde::Deserializer::from_read_ref(next);
        output.transcode_from(&mut de)?;
        buf = rest;
      }
    }
    Input::Unbuffered(r) => {
      // Note that in reader mode, the MessagePack deserializer will eagerly
      // allocate zero-filled buffers for binary and string data based on the
      // length specified in the input. Our dirty "solution" is to document in
      // the help output that jyt is not designed for use with untrusted input.
      let mut r = BufReader::new(r);
      while has_data_left(&mut r)? {
        let mut de = rmp_serde::Deserializer::new(&mut r);
        output.transcode_from(&mut de)?;
      }
    }
  }
  Ok(())
}

fn has_data_left<R>(r: &mut BufReader<R>) -> io::Result<bool>
where
  R: Read,
{
  r.fill_buf().map(|b| !b.is_empty())
}

pub struct Output<W: Write>(W);

impl<W: Write> Output<W> {
  pub fn new(w: W) -> Output<W> {
    Output(w)
  }
}

impl<W: Write> crate::Output for Output<W> {
  fn transcode_from<'de, D, E>(&mut self, de: D) -> Result<(), Box<dyn Error>>
  where
    D: serde::de::Deserializer<'de, Error = E>,
    E: serde::de::Error + 'static,
  {
    let mut ser = rmp_serde::Serializer::new(&mut self.0);
    transcode::transcode(de, &mut ser)?;
    Ok(())
  }

  fn transcode_value<S>(&mut self, value: S) -> Result<(), Box<dyn Error>>
  where
    S: serde::ser::Serialize,
  {
    value.serialize(&mut rmp_serde::Serializer::new(&mut self.0))?;
    Ok(())
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
/// # Examples
///
/// ```
/// # // NOTE: This test case is copied to `test_doc_example` below.
/// # // See https://github.com/rust-lang/rust/issues/50784.
/// let input = [0xa3, b'j', b'y', b't']; // the string "jyt"
/// assert_eq!(next_value_size(&input), Ok(4));
/// ```
fn next_value_size(input: &[u8]) -> Result<usize, ReadSizeError> {
  use rmp::Marker::*;

  if input.len() < 1 {
    return Ok(0);
  }

  let marker = rmp::Marker::from_u8(input[0]);
  let size_after_marker = match marker {
    Reserved => return Err(ReadSizeError::InvalidMarker),
    Null | True | False | FixPos(_) | FixNeg(_) => 0,
    U8 | I8 => 1,
    U16 | I16 => 2,
    U32 | I32 | F32 => 4,
    U64 | I64 | F64 => 8,
    FixExt1 => 2,
    FixExt2 => 3,
    FixExt4 => 5,
    FixExt8 => 9,
    FixExt16 => 17,
    Ext8 => 2 + try_read_length::<u8>(input)? as usize,
    Ext16 => 3 + try_read_length::<u16>(input)? as usize,
    Ext32 => 5 + try_read_length::<u32>(input)? as usize,
    FixStr(n) => n as usize,
    Str8 | Bin8 => 1 + try_read_length::<u8>(input)? as usize,
    Str16 | Bin16 => 2 + try_read_length::<u16>(input)? as usize,
    Str32 | Bin32 => 4 + try_read_length::<u32>(input)? as usize,
    FixArray(count) => total_sequence_size(&input[1..], count as u64)?,
    Array16 => {
      let count = try_read_length::<u16>(input)? as u64;
      let seq = input.get(3..).ok_or(ReadSizeError::Truncated)?;
      2 + total_sequence_size(seq, count)?
    }
    Array32 => {
      let count = try_read_length::<u32>(input)? as u64;
      let seq = input.get(5..).ok_or(ReadSizeError::Truncated)?;
      4 + total_sequence_size(seq, count)?
    }
    FixMap(pair_count) => total_sequence_size(&input[1..], pair_count as u64 * 2)?,
    Map16 => {
      let pair_count = try_read_length::<u16>(input)? as u64;
      let seq = input.get(3..).ok_or(ReadSizeError::Truncated)?;
      2 + total_sequence_size(seq, pair_count * 2)?
    }
    Map32 => {
      let pair_count = try_read_length::<u32>(input)? as u64;
      let seq = input.get(5..).ok_or(ReadSizeError::Truncated)?;
      4 + total_sequence_size(seq, pair_count * 2)?
    }
  };

  let total_size: usize = 1 + size_after_marker;
  if total_size <= input.len() {
    Ok(total_size)
  } else {
    Err(ReadSizeError::Truncated)
  }
}

fn total_sequence_size(input: &[u8], count: u64) -> Result<usize, ReadSizeError> {
  let mut total = 0;
  let mut seq = input;

  for _ in 0..count {
    if seq.len() == 0 {
      return Err(ReadSizeError::Truncated);
    }
    let size = next_value_size(seq)?;
    total += size;
    seq = &seq[size..];
  }

  Ok(total)
}

fn try_read_length<'a, T>(input: &'a [u8]) -> Result<T, ReadSizeError>
where
  &'a [u8]: TryReadPrefix<T>,
{
  match <&[u8] as TryReadPrefix<T>>::try_read_prefix(&input[1..]) {
    Some(n) => Ok(n),
    None => Err(ReadSizeError::Truncated),
  }
}

trait TryReadPrefix<T> {
  fn try_read_prefix(self) -> Option<T>;
}

macro_rules! __impl_try_read_u8_slice_prefix {
  ($t:ty) => {
    impl TryReadPrefix<$t> for &[u8] {
      fn try_read_prefix(self) -> Option<$t> {
        const SIZE: usize = std::mem::size_of::<$t>();
        match self.get(..SIZE)?.try_into() {
          Ok(arr) => Some(<$t>::from_be_bytes(arr)),
          Err(_) => None,
        }
      }
    }
  };
}
__impl_try_read_u8_slice_prefix!(u8);
__impl_try_read_u8_slice_prefix!(u16);
__impl_try_read_u8_slice_prefix!(u32);

/// The error type returned by [`next_value_size`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ReadSizeError {
  /// A MessagePack value in the input was truncated.
  Truncated,
  /// A MessagePack value in the input contained the reserved marker byte 0xc1.
  InvalidMarker,
}

impl Error for ReadSizeError {}

impl Display for ReadSizeError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use ReadSizeError::*;
    match self {
      Truncated => write!(f, "unexpected end of MessagePack input"),
      InvalidMarker => write!(f, "invalid MessagePack marker in input"),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ReadSizeError::*;

  use hex_literal::hex;

  const VALID_INPUTS: &[&[u8]] = &[
    // empty
    &[],
    // nil
    &hex!("c0"),
    // bool format family
    &hex!("c2"), // false
    &hex!("c3"), // true
    // int format family
    &hex!("2a"),                         // positive fixint
    &hex!("f4"),                         // negative fixint
    &hex!("cc 09"),                      // 8-bit unsigned
    &hex!("cd 09 f9"),                   // 16-bit unsigned
    &hex!("ce 09 f9 11 02"),             // 32-bit unsigned
    &hex!("cf 09 f9 11 02 9d 74 e3 5b"), // 64-bit unsigned
    &hex!("d0 d8"),                      // 8-bit signed
    &hex!("d1 d8 41"),                   // 16-bit signed
    &hex!("d2 d8 41 56 c5"),             // 32-bit signed
    &hex!("d3 d8 41 56 c5 63 56 88 c0"), // 64-bit signed
    // float format family
    &hex!("ca 64 7a 5a 6e"),             // single precision
    &hex!("cb 54 79 4b 50 45 67 4e 64"), // double precision
    // str format family: "jyt"
    &hex!("a3 6a 79 74"),             // fixstr
    &hex!("d9 03 6a 79 74"),          // str 8
    &hex!("da 00 03 6a 79 74"),       // str 16
    &hex!("db 00 00 00 03 6a 79 74"), // str 32
    // str format family: ""
    &hex!("a0"),             // fixstr
    &hex!("d9 00"),          // str 8
    &hex!("da 00 00"),       // str 16
    &hex!("db 00 00 00 00"), // str 32
    // bin format family: b"jyt"
    &hex!("c4 03 6a 79 74"),          // bin 8
    &hex!("c5 00 03 6a 79 74"),       // bin 16
    &hex!("c6 00 00 00 03 6a 79 74"), // bin 32
    // bin format family: b""
    &hex!("c4 00"),          // bin 8
    &hex!("c5 00 00"),       // bin 16
    &hex!("c6 00 00 00 00"), // bin 32
    // array format family: ["jyt", true]
    &hex!("92 a3 6a 79 74 c3"),             // fixarray
    &hex!("dc 00 02 a3 6a 79 74 c3"),       // array 16
    &hex!("dd 00 00 00 02 a3 6a 79 74 c3"), // array 32
    // array format family: []
    &hex!("90"),             // fixarray
    &hex!("dc 00 00"),       // array 16
    &hex!("dd 00 00 00 00"), // array 32
    // map format family: {"jyt": true, "good": true}
    &hex!("82 a3 6a 79 74 c3 a4 67 6f 6f 64 c3"), // fixmap
    &hex!("de 00 02 a3 6a 79 74 c3 a4 67 6f 6f 64 c3"), // map 16
    &hex!("df 00 00 00 02 a3 6a 79 74 c3 a4 67 6f 6f 64 c3"), // map 32
    // map format family: {}
    &hex!("80"),             // fixmap
    &hex!("de 00 00"),       // map 16
    &hex!("df 00 00 00 00"), // map 32
    // ext format family
    &hex!("d4 01 09"),                                              // fixext 1
    &hex!("d5 01 09 f9"),                                           // fixext 2
    &hex!("d6 01 09 f9 11 02"),                                     // fixext 4
    &hex!("d7 01 09 f9 11 02 9d 74 e3 5b"),                         // fixext 8
    &hex!("d8 01 09 f9 11 02 9d 74 e3 5b d8 41 56 c5 63 56 88 c0"), // fixext 16
    &hex!("c7 04 01 09 f9 11 02"),                                  // ext 8
    &hex!("c8 00 04 01 09 f9 11 02"),                               // ext 16
    &hex!("c9 00 00 00 04 01 09 f9 11 02"),                         // ext 32
  ];

  #[test]
  fn test_valid_inputs() {
    for input in VALID_INPUTS {
      assert_eq!(next_value_size(input), Ok(input.len()));
    }
  }

  #[test]
  fn test_doc_example() {
    let input = [0xa3, b'j', b'y', b't']; // the string "jyt"
    assert_eq!(next_value_size(&input), Ok(4));
  }

  #[test]
  fn test_truncated_valid_inputs() {
    for input in VALID_INPUTS.iter().filter(|i| i.len() > 1) {
      for len in 1..(input.len() - 1) {
        assert_eq!(next_value_size(&input[..len]), Err(Truncated))
      }
    }
  }

  #[test]
  fn test_nonsensically_large_input() {
    // The string "jyt," but with a reported length of 2^32-1 bytes.
    assert_eq!(
      next_value_size(&hex!("db ff ff ff ff 6a 79 74")),
      Err(Truncated)
    );
  }

  #[test]
  fn test_invalid_marker() {
    // <invalid>
    assert_eq!(next_value_size(&hex!("c1")), Err(InvalidMarker));
    // ["jyt", <invalid>]
    assert_eq!(
      next_value_size(&hex!("92 a3 6a 79 74 c1")),
      Err(InvalidMarker)
    );
    // {"jyt": true, "good": <invalid>}
    assert_eq!(
      next_value_size(&hex!("82 a3 6a 79 74 c3 a4 67 6f 6f 64 c1")),
      Err(InvalidMarker)
    );
  }

  #[test]
  fn test_suffixes_skipped() {
    // true; <invalid>
    assert_eq!(next_value_size(&hex!("c3 c1")), Ok(1));
    // ["jyt"]; <invalid>
    assert_eq!(next_value_size(&hex!("91 a3 6a 79 74 c1")), Ok(5));
  }
}
