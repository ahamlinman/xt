use std::io::{self, Read};
use std::ops::Deref;
use std::rc::Rc;

/// An opaque handle to serialized input data from a buffer or reader source.
///
/// While either kind of input is fully supported and will produce the same
/// output for the same input data, xt may be able to modify or optimize its
/// internal behavior in useful ways based on the kind of input provided.
pub struct InputHandle<'a>(Input<'a>);

/// A container for the program's original input data.
pub(crate) enum Input<'a> {
  Buffer(Rc<dyn Deref<Target = [u8]> + 'a>),
  Reader(Box<dyn Read + 'a>),
}

impl<'a> From<InputHandle<'a>> for Input<'a> {
  fn from(handle: InputHandle<'a>) -> Input<'a> {
    handle.0
  }
}

impl<'a> InputHandle<'a> {
  /// Creates a handle for an input buffer.
  pub fn from_buffer<B>(buf: B) -> InputHandle<'a>
  where
    B: Deref<Target = [u8]> + 'a,
  {
    InputHandle(Input::Buffer(Rc::new(buf)))
  }

  /// Creates a handle for an input reader.
  ///
  /// Use of a reader handle does not guarantee that xt will process input in
  /// streaming fashion, as some input formats and xt features require buffered
  /// input.
  ///
  /// If possible, the reader should avoid performing its own buffering. For
  /// example, a [`std::fs::File`] is preferable to a [`std::io::BufReader`]
  /// wrapping a file.
  pub fn from_reader<R>(r: R) -> InputHandle<'a>
  where
    R: Read + 'a,
  {
    InputHandle(Input::Reader(Box::new(r)))
  }

  /// Returns the input as a slice, transforming `self` into a buffer handle if
  /// it is currently a reader handle.
  pub(crate) fn try_as_buffer(&mut self) -> io::Result<&[u8]> {
    self.ensure_buffered()?;
    match &self.0 {
      Input::Buffer(buf) => Ok(buf),
      Input::Reader(_) => unreachable!(),
    }
  }

  /// Returns a handle to the input as a buffer, transforming `self` into a
  /// buffer handle if it is currently a reader handle.
  pub(crate) fn try_clone(&mut self) -> io::Result<InputHandle> {
    self.ensure_buffered()?;
    match &mut self.0 {
      Input::Buffer(buf) => Ok(InputHandle(Input::Buffer(Rc::clone(buf)))),
      Input::Reader(_) => unreachable!(),
    }
  }

  fn ensure_buffered(&mut self) -> io::Result<()> {
    if let Input::Reader(r) = &mut self.0 {
      let mut buf = vec![];
      r.read_to_end(&mut buf)?;
      self.0 = Input::Buffer(Rc::new(buf));
    }
    Ok(())
  }
}
