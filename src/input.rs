use std::convert::Into;
use std::fs::File;
use std::io::{self, Read};
use std::ops::Deref;
use std::path::Path;
use std::rc::Rc;

use memmap2::MmapOptions;

/// A container for the program's input data, which may be a buffer or an unused
/// reader.
pub enum Input {
  Buffered(Rc<dyn Deref<Target = [u8]>>),
  Unbuffered(Box<dyn Read>),
}

/// An opaque, possibly shared reference to the program's original input data.
///
/// An unbuffered reference contains an unused reader that will provide the
/// input as a stream, while a buffered reference contains the entire input as a
/// slice. Any operation that requires a buffer, such as cloning the reference
/// to read input more than once, will transform an unbuffered reference into a
/// buffered reference if necessary by reading all input into a byte vector.
pub struct InputRef(Input);

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

  /// Attempts to create a buffered reference by mmap-ing the file at the
  /// provided path. If this fails (for example, if the file is something like a
  /// named pipe), creates an unbuffered reference using the open file as a
  /// reader.
  pub fn from_file<P>(path: P) -> io::Result<InputRef>
  where
    P: AsRef<Path>,
  {
    let file = File::open(&path)?;
    // Safety: Modification of the mapped file outside the process triggers
    // undefined behavior. Our dirty "solution" is to document this in the
    // help output.
    match unsafe { MmapOptions::new().populate().map(&file) } {
      // Per memmap2 docs, it's safe to drop file once mmap succeeds.
      Ok(map) => Ok(InputRef(Input::Buffered(Rc::new(map)))),
      Err(_) => Ok(InputRef(Input::Unbuffered(Box::new(file)))),
    }
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
