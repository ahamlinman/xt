use std::convert::Into;
use std::io::{self, Read};
use std::ops::Deref;
use std::rc::Rc;

/// An opaque reference to serialized input data.
pub struct InputRef(Input);

/// A container for the program's original input data.
pub(crate) enum Input {
  /// The entire contents of the input as a byte slice.
  Buffer(Rc<dyn Deref<Target = [u8]>>),
  /// A reader that will provide the input as a stream. Users should assume that
  /// the reader does not perform its own buffering, and consider wrapping with
  /// [`std::io::BufReader`] or similar before use.
  Reader(Box<dyn Read>),
}

impl Into<Input> for InputRef {
  /// Extracts the referenced input.
  fn into(self) -> Input {
    self.0
  }
}

impl InputRef {
  /// Creates a reference to an input reader.
  ///
  /// If possible, the reader should avoid performing its own buffering. For
  /// example, a [`std::fs::File`] is preferable to a [`std::io::BufReader`]
  /// wrapping a file.
  pub fn from_reader<R>(r: R) -> InputRef
  where
    R: Read + 'static,
  {
    InputRef(Input::Reader(Box::new(r)))
  }

  /// Creates a reference to an input buffer.
  pub fn from_buffer<B>(buf: B) -> InputRef
  where
    B: Deref<Target = [u8]> + 'static,
  {
    InputRef(Input::Buffer(Rc::new(buf)))
  }

  /// Returns the input as a slice, transforming `self` into a buffer reference
  /// if it is currently a reader reference.
  pub(crate) fn try_buffer(&mut self) -> io::Result<&(dyn Deref<Target = [u8]>)> {
    self.ensure_buffered()?;
    match &self.0 {
      Input::Buffer(buf) => Ok(buf.as_ref()),
      Input::Reader(_) => unreachable!(),
    }
  }

  /// Returns a buffered reference to the input, transforming `self` into a
  /// buffer reference if it is currently a reader reference.
  pub(crate) fn try_clone(&mut self) -> io::Result<InputRef> {
    self.ensure_buffered()?;
    match &mut self.0 {
      Input::Buffer(buf) => Ok(InputRef(Input::Buffer(Rc::clone(buf)))),
      Input::Reader(_) => unreachable!(),
    }
  }

  fn ensure_buffered(&mut self) -> io::Result<()> {
    self.0 = match &mut self.0 {
      Input::Buffer(_) => return Ok(()),
      Input::Reader(r) => {
        let mut buf = Vec::new();
        r.read_to_end(&mut buf)?;
        Input::Buffer(Rc::new(buf))
      }
    };
    Ok(())
  }
}
