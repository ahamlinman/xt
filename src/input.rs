use std::convert::Into;
use std::io::{self, Read};
use std::ops::Deref;
use std::rc::Rc;

/// An opaque, possibly shared reference to the program's original input data.
///
/// An unbuffered reference contains an unused reader that will provide the
/// input as a stream, while a buffered reference contains the entire input as a
/// slice. Any operation that requires a buffer, such as cloning the reference
/// to read input more than once, will transform an unbuffered reference into a
/// buffered reference if necessary by reading all input into a byte vector.
///
/// Readers should avoid internal buffering. Clients that extract a reader from
/// an unbuffered reference should consider wrapping with [`std::io::BufReader`]
/// before use.
pub struct InputRef(Input);

/// A container for the program's input data, which may be a buffer or an unused
/// reader.
pub enum Input {
  Buffered(Rc<dyn Deref<Target = [u8]>>),
  Unbuffered(Box<dyn Read>),
}

impl Into<Input> for InputRef {
  /// Extracts the referenced input.
  fn into(self) -> Input {
    self.0
  }
}

impl InputRef {
  /// Creates an unbuffered reference to input from the provided reader.
  pub fn from_reader<R>(r: R) -> InputRef
  where
    R: Read + 'static,
  {
    InputRef(Input::Unbuffered(Box::new(r)))
  }

  /// Creates a buffered reference to input from the provided buffer.
  pub fn from_buffer<B>(buf: B) -> InputRef
  where
    B: Deref<Target = [u8]> + 'static,
  {
    InputRef(Input::Buffered(Rc::new(buf)))
  }

  /// Returns the input as a buffered slice, transforming `self` into a buffered
  /// reference if it is currently unbuffered.
  pub fn try_buffer(&mut self) -> io::Result<&(dyn Deref<Target = [u8]>)> {
    self.ensure_buffered()?;
    match &self.0 {
      Input::Buffered(buf) => Ok(buf.as_ref()),
      Input::Unbuffered(_) => unreachable!(),
    }
  }

  /// Returns a buffered reference to the input, transforming `self` into a
  /// buffered reference if it is currently unbuffered.
  pub fn try_clone(&mut self) -> io::Result<InputRef> {
    self.ensure_buffered()?;
    match &mut self.0 {
      Input::Buffered(buf) => Ok(InputRef(Input::Buffered(Rc::clone(buf)))),
      Input::Unbuffered(_) => unreachable!(),
    }
  }

  fn ensure_buffered(&mut self) -> io::Result<()> {
    self.0 = match &mut self.0 {
      Input::Buffered(_) => return Ok(()),
      Input::Unbuffered(r) => {
        let mut buf = Vec::new();
        r.read_to_end(&mut buf)?;
        Input::Buffered(Rc::new(buf))
      }
    };
    Ok(())
  }
}
