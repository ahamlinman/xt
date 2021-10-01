use std::io::{self, Read};
use std::ops::Deref;
use std::rc::Rc;

/// An opaque handle to serialized input data from a buffer or reader source.
///
/// While either kind of input is fully supported and will produce the same
/// output for the same input data, jyt may be able to modify or optimize its
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
  /// Use of a reader handle does not guarantee that jyt will process input in
  /// streaming fashion, as some input formats and jyt features require buffered
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
  pub(crate) fn try_buffer(&mut self) -> io::Result<&(dyn Deref<Target = [u8]>)> {
    self.ensure_buffered()?;
    match &self.0 {
      Input::Buffer(buf) => Ok(buf.as_ref()),
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
