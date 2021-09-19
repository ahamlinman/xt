use std::convert::Into;
use std::fs::File;
use std::io::{self, Read};
use std::ops::Deref;
use std::path::Path;
use std::rc::Rc;

use memmap2::MmapOptions;

pub enum Input {
  Buffered(Rc<dyn Deref<Target = [u8]>>),
  Unbuffered(Box<dyn Read>),
}

pub struct InputRef(Input);

impl Into<Input> for InputRef {
  fn into(self) -> Input {
    self.0
  }
}

impl InputRef {
  pub fn from_reader<R>(r: R) -> InputRef
  where
    R: Read + 'static,
  {
    InputRef(Input::Unbuffered(Box::new(r)))
  }

  pub fn from_file<P>(path: P) -> io::Result<InputRef>
  where
    P: AsRef<Path>,
  {
    let file = File::open(&path)?;
    // Safety: Modification of the mapped file outside the process triggers
    // undefined behavior. Our dirty "solution" is to document this in the
    // help output.
    match unsafe { MmapOptions::new().populate().map(&file) } {
      // Per memmap2 docs, it's safe to drop the file once mmap succeeds.
      Ok(map) => Ok(InputRef(Input::Buffered(Rc::new(map)))),
      // If mmap fails, we can still try regular buffering.
      Err(_) => Ok(InputRef(Input::Unbuffered(Box::new(file)))),
    }
  }

  pub fn try_buffer(&mut self) -> io::Result<&(dyn Deref<Target = [u8]>)> {
    self.ensure_buffered()?;
    match &self.0 {
      Input::Buffered(buf) => Ok(buf.as_ref()),
      Input::Unbuffered(_) => unreachable!(),
    }
  }

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
