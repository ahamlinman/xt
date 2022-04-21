use std::borrow::Cow;
use std::io::{self, Cursor, Read, Write};

/// A handle for xt to obtain input data from a slice or reader.
pub struct InputHandle<'i>(Source<'i>);

/// A private container representing a buffer or reader input source.
enum Source<'i> {
  Buffer(Cow<'i, [u8]>),
  Reader(RewindableReader<Box<dyn Read + 'i>>),
}

impl<'i> InputHandle<'i> {
  /// Creates a handle for an input slice.
  pub fn from_slice(source: &'i [u8]) -> InputHandle<'i> {
    InputHandle(Source::Buffer(Cow::Borrowed(source)))
  }

  /// Creates a handle for an input reader.
  pub fn from_reader<R>(source: R) -> InputHandle<'i>
  where
    R: Read + 'i,
  {
    InputHandle(Source::Reader(RewindableReader::new(Box::new(source))))
  }

  /// Returns temporary references to the handle's input.
  ///
  /// A borrowed reader will always produce the original input from the start,
  /// even across multiple calls to `borrow_mut`.
  pub(crate) fn borrow_mut(&mut self) -> BorrowedInput<'i, '_> {
    match &mut self.0 {
      Source::Buffer(buf) => BorrowedInput::Buffer(buf),
      Source::Reader(r) => BorrowedInput::Reader(ReaderGuard(r)),
    }
  }
}

/// A temporary reference to input held by an [`InputHandle`].
pub(crate) enum BorrowedInput<'i, 'h>
where
  'i: 'h,
{
  Buffer(&'h [u8]),
  Reader(ReaderGuard<'i, 'h>),
}

impl<'i, 'h> Read for BorrowedInput<'i, 'h>
where
  'i: 'h,
{
  fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
    match self {
      BorrowedInput::Buffer(b) => b.read(buf),
      BorrowedInput::Reader(r) => r.read(buf),
    }
  }
}

/// A temporary reference to input from a reader.
///
/// A [`ReaderGuard`] automatically captures all bytes read from the original
/// input. When dropped, it will rewind to the start of the captured input so
/// that future consumers read the same bytes.
pub(crate) struct ReaderGuard<'i, 'h>(&'h mut RewindableReader<Box<dyn Read + 'i>>)
where
  'i: 'h;

impl<'i, 'h> Read for ReaderGuard<'i, 'h>
where
  'i: 'h,
{
  fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
    self.0.read(buf)
  }
}

impl<'i, 'h> Drop for ReaderGuard<'i, 'h>
where
  'i: 'h,
{
  fn drop(&mut self) {
    self.0.rewind();
  }
}

impl<'i> TryInto<Cow<'i, [u8]>> for InputHandle<'i> {
  type Error = io::Error;

  fn try_into(self) -> io::Result<Cow<'i, [u8]>> {
    match self.0 {
      Source::Buffer(buf) => Ok(buf),
      Source::Reader(r) => {
        if r.is_source_eof() {
          let (cursor, _) = r.into_inner();
          return Ok(Cow::Owned(cursor.into_inner()));
        }

        let (cursor, mut source) = r.into_inner();
        let mut buf = cursor.into_inner();
        source.read_to_end(&mut buf)?;
        Ok(Cow::Owned(buf))
      }
    }
  }
}

/// A direct reference to the original input held by an [`InputHandle`].
pub(crate) enum Input<'i> {
  Buffer(Cow<'i, [u8]>),
  Reader(Box<dyn Read + 'i>),
}

impl<'i> From<InputHandle<'i>> for Input<'i> {
  fn from(handle: InputHandle<'i>) -> Self {
    match handle.0 {
      Source::Buffer(buf) => Input::Buffer(buf),
      Source::Reader(r) => {
        let source_eof = r.is_source_eof();
        let (cursor, source) = r.into_inner();
        if source_eof {
          Input::Buffer(Cow::Owned(cursor.into_inner()))
        } else if cursor.get_ref().is_empty() {
          Input::Reader(source)
        } else {
          Input::Reader(Box::new(cursor.chain(source)))
        }
      }
    }
  }
}

/// A reader that adds backwards-only seeking to a non-seekable source.
///
/// As a rewindable reader pulls bytes from its source, it captures them in an
/// in-memory buffer. A call to [`rewind()`][RewindableReader::rewind] will
/// cause the reader to produce the source's data from the beginning. After
/// producing the captured data from previous reads, future reads will continue
/// to extend the capture buffer from the original source.
pub(crate) struct RewindableReader<R>
where
  R: Read,
{
  cursor: Cursor<Vec<u8>>,
  source: R,
  source_eof: bool,
}

impl<R> RewindableReader<R>
where
  R: Read,
{
  /// Returns a new rewindable reader wrapping the provided source reader.
  fn new(source: R) -> Self {
    Self {
      cursor: Cursor::new(vec![]),
      source,
      source_eof: false,
    }
  }

  /// Rewinds the reader so that subsequent reads will produce the source's data
  /// from the beginning.
  fn rewind(&mut self) {
    self.cursor.set_position(0);
  }

  /// Returns true if the latest read from the source indicated an "end of file"
  /// condition.
  fn is_source_eof(&self) -> bool {
    self.source_eof
  }

  /// Consumes the reader, returning any captured prefix as well as the source
  /// reader.
  fn into_inner(self) -> (Cursor<Vec<u8>>, R) {
    (self.cursor, self.source)
  }
}

impl<R> Read for RewindableReader<R>
where
  R: Read,
{
  fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
    // First, copy as much data as we can from the unread portion of the Cursor
    // into the buffer.
    let prefix_size = std::cmp::min(buf.len(), self.cursor_unread());
    self.cursor.read_exact(&mut buf[..prefix_size])?;
    if self.cursor_unread() > 0 || prefix_size == buf.len() {
      return Ok(prefix_size);
    }

    // Second, fill the rest of the buffer with data from the source, and
    // capture it for ourselves as well.
    //
    // The `read` documentation recommends against reading the contents of buf.
    // Reading between the lines, though, it seems the main goal is to prevent
    // the accidental use of uninitialized memory, and not to handle exotic
    // situations like "buf might point to memory mapped hardware that doesn't
    // act like RAM." As buf is &mut we can assume nobody else aliases it, and
    // we are only reading a slice of data we know we've written ourselves, so
    // uninitialized memory should not be a problem for our particular usage.
    let buf = &mut buf[prefix_size..];
    let source_size = self.source.read(buf)?;
    self.cursor.write_all(&buf[..source_size])?;

    // Finally, mark whether the source has reached EOF. Since we return early
    // when prefix_size == buf.len(), and prefix_size == 0 for buf.len() == 0
    // (as prefix_size is a min of unsized values), we know that a source read
    // of 0 bytes must indicate EOF rather than an empty buffer.
    if source_size == 0 {
      self.source_eof = true;
    }

    Ok(prefix_size + source_size)
  }
}

impl<R> RewindableReader<R>
where
  R: Read,
{
  fn cursor_unread(&self) -> usize {
    let offset = self.cursor.position() as usize;
    self.cursor.get_ref().len() - offset
  }
}

#[cfg(test)]
mod tests {
  use super::RewindableReader;
  use std::io::{Cursor, Read};

  const DATA: &str = "abcdefghij";

  #[test]
  fn rewindable_reader_straight_read() {
    let mut r = RewindableReader::new(Cursor::new(String::from(DATA)));

    let mut result = String::new();
    assert!(matches!(
      r.read_to_string(&mut result),
      Ok(len) if len == DATA.len(),
    ));
    assert_eq!(result, DATA);
    assert!(r.is_source_eof());

    let (cursor, _) = r.into_inner();
    assert!(matches!(std::str::from_utf8(cursor.get_ref()), Ok(DATA)));
  }

  #[test]
  fn rewinding_rewindable_reader() {
    let mut r = RewindableReader::new(Cursor::new(String::from(DATA)));

    const HALF: usize = DATA.len() / 2;
    let mut tmp = [0; HALF];
    assert!(matches!(r.read_exact(&mut tmp), Ok(())));
    assert_eq!(std::str::from_utf8(&tmp), Ok(&DATA[..HALF]));
    assert!(!r.is_source_eof());

    r.rewind();

    let mut result = String::new();
    assert!(matches!(r.read_to_string(&mut result), Ok(len) if len == DATA.len()));
    assert_eq!(result, DATA);
    assert!(r.is_source_eof());
  }
}
