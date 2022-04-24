use std::borrow::Cow;
use std::io::{self, Cursor, Read, Write};

/// A container for input to [`xt::translate`](crate::translate).
///
/// xt accepts input from both slices and readers, and will produce consistent
/// output for a given input regardless of its source. However, xt may optimize
/// its behavior or provide features depending on the kind of source used. See
/// [`Handle::from_slice`] and [`Handle::from_reader`] for details.
pub struct Handle<'i>(Source<'i>);

/// The private container for the original input a [`Handle`] was created from.
enum Source<'i> {
  Slice(&'i [u8]),
  Reader(CaptureReader<Box<dyn Read + 'i>>),
}

impl<'i> Handle<'i> {
  /// Creates a handle for an input slice.
  ///
  /// Slice inputs are typically more efficient to translate than reader inputs,
  /// but require all input to be loaded into memory in advance. This may be
  /// inappropriate for an unbounded stream of documents in a format that
  /// supports streaming translation.
  pub fn from_slice(source: &'i [u8]) -> Handle<'i> {
    Handle(Source::Slice(source))
  }

  /// Creates a handle for an input reader.
  ///
  /// Reader inputs enable streaming translation for select input formats,
  /// allowing xt to translate documents as they appear in the stream without
  /// buffering more than one document in memory at a time. When translating
  /// from a format that does not support streaming, xt will buffer the entire
  /// contents of the reader into memory before starting translation.
  pub fn from_reader<R>(source: R) -> Handle<'i>
  where
    R: Read + 'i,
  {
    Handle(Source::Reader(CaptureReader::new(Box::new(source))))
  }

  /// Borrows a temporary reference to the input.
  ///
  /// For slice inputs, this provides access to the original slice.
  ///
  /// For reader inputs that are fully buffered from previous use of the handle,
  /// this provides access to the reader's full contents as a slice.
  ///
  /// For reader inputs not yet fully buffered, this provides access to the
  /// reader through a wrapper that captures its output. In subsequent calls to
  /// `borrow_mut`, the wrapper will produce the captured bytes before producing
  /// more bytes from the original reader.
  pub(crate) fn borrow_mut(&mut self) -> Ref<'i, '_> {
    match &mut self.0 {
      Source::Slice(b) => Ref::Slice(b),
      Source::Reader(r) => {
        if r.is_source_eof() {
          Ref::Slice(r.captured())
        } else {
          Ref::Reader(ReaderGuard(r))
        }
      }
    }
  }
}

/// Produces the original input as a slice, either by passing through the
/// original slice or fully reading the original reader into a buffer.
impl<'i> TryInto<Cow<'i, [u8]>> for Handle<'i> {
  type Error = io::Error;

  fn try_into(self) -> io::Result<Cow<'i, [u8]>> {
    match self.0 {
      Source::Slice(b) => Ok(Cow::Borrowed(b)),
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

/// A container for owned input, created by consuming a [`Handle`].
///
/// The kind of `Input` produced from a `Handle` may not correspond directly to
/// the original `Source`. If a reader input was fully buffered through normal
/// use of the `Handle`, the `Input` will provide ownership of the buffer, and
/// the conversion from `Handle` will drop the reader.
pub(crate) enum Input<'i> {
  Slice(Cow<'i, [u8]>),
  Reader(Box<dyn Read + 'i>),
}

impl<'i> From<Handle<'i>> for Input<'i> {
  fn from(handle: Handle<'i>) -> Self {
    match handle.0 {
      Source::Slice(b) => Input::Slice(Cow::Borrowed(b)),
      Source::Reader(r) => {
        let source_eof = r.is_source_eof();
        let (cursor, source) = r.into_inner();
        if source_eof {
          Input::Slice(Cow::Owned(cursor.into_inner()))
        } else if cursor.get_ref().is_empty() {
          Input::Reader(source)
        } else {
          Input::Reader(Box::new(cursor.chain(source)))
        }
      }
    }
  }
}

/// A temporary reference to input from a [`Handle`].
///
/// See [`Handle::borrow_mut`] for more.
pub(crate) enum Ref<'i, 'h>
where
  'i: 'h,
{
  Slice(&'h [u8]),
  Reader(ReaderGuard<'i, 'h>),
}

impl<'i, 'h> Ref<'i, 'h>
where
  'i: 'h,
{
  /// Returns the full input as a slice.
  ///
  /// For reader inputs not yet fully buffered, this will immediately consume
  /// all remaining bytes from the reader into memory.
  pub(crate) fn slice(&mut self) -> io::Result<&[u8]> {
    match self {
      Ref::Slice(b) => Ok(b),
      Ref::Reader(r) => {
        r.0.capture_to_end()?;
        Ok(r.0.captured())
      }
    }
  }

  /// Returns a prefix of the input.
  ///
  /// For slice inputs and fully buffered reader inputs, this simply returns the
  /// full input.
  ///
  /// For reader inputs not yet fully buffered, `want_size` represents the
  /// minimum size of the prefix that the call should attempt to produce by
  /// capturing new bytes from the source. The returned prefix may be smaller or
  /// larger than `want_size` if the reader reaches EOF, more input has already
  /// been captured, or a larger read is made from the source for efficiency.
  pub(crate) fn prefix(&mut self, want_size: usize) -> io::Result<&[u8]> {
    match self {
      Ref::Slice(b) => Ok(b),
      Ref::Reader(r) => {
        r.0.capture_to_size(want_size)?;
        Ok(r.0.captured())
      }
    }
  }
}

/// Wraps a [`CaptureReader`] to rewind its position to the start when dropped.
///
/// This enables multiple calls to [`Handle::borrow_mut`] to access reader input
/// without fully consuming it.
pub(crate) struct ReaderGuard<'i, 'h>(&'h mut CaptureReader<Box<dyn Read + 'i>>)
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

/// A wrapper that captures the output of a reader into a buffer as it is read.
///
/// A `CaptureReader` provides limited seeking capabilities for a non-seekable
/// reader by storing a copy of the bytes it produces for each `read` call.
/// After calling [`rewind`][CaptureReader::rewind], the `CaptureReader` will
/// produce the stored bytes for new `read` calls before consuming more of the
/// source, as if [`Seek::rewind`][std::io::Seek::rewind] had been used on the
/// source (except that rewinding a `CaptureReader` is infallible).
///
/// A `CaptureReader` also tracks when the source reports an "end of file"
/// condition, indicating that the source is fully buffered as if
/// [`read_to_end`][Read::read_to_end] had been used. Consuming the
/// `CaptureReader` with [`into_inner`][CaptureReader::into_inner] permits
/// access to the buffered bytes without additional copies.
pub(crate) struct CaptureReader<R>
where
  R: Read,
{
  prefix: Cursor<Vec<u8>>,
  source: R,
  source_eof: bool,
}

impl<R> CaptureReader<R>
where
  R: Read,
{
  /// Creates a new reader that captures the bytes produced by `source`.
  fn new(source: R) -> Self {
    Self {
      prefix: Cursor::new(vec![]),
      source,
      source_eof: false,
    }
  }

  /// Rewinds the reader so that subsequent reads will produce the source's
  /// bytes from the beginning.
  fn rewind(&mut self) {
    self.prefix.set_position(0);
  }

  /// Captures all of the source's remaining input without modifying the
  /// reader's position.
  fn capture_to_end(&mut self) -> io::Result<()> {
    self.source.read_to_end(self.prefix.get_mut())?;
    self.source_eof = true;
    Ok(())
  }

  /// Ensures that at least `size` bytes have been captured from the source
  /// without modifying the reader's position.
  ///
  /// The actual number of captured bytes may be less than `size` if the source
  /// reaches EOF before producing `size` bytes.
  fn capture_to_size(&mut self, size: usize) -> io::Result<()> {
    // This matches the privately defined default size of a BufReader for most
    // platforms as of this writing. It seems like a reasonable enough lower
    // bound to prevent us from spending a system call on, say, one byte.
    const MIN_SIZE: usize = 8 * 1024;

    let needed = std::cmp::max(size, MIN_SIZE).saturating_sub(self.prefix.get_ref().len());
    if needed == 0 {
      return Ok(());
    }

    let mut take = (&mut self.source).take(needed as u64);
    take.read_to_end(self.prefix.get_mut())?;
    if take.limit() > 0 {
      self.source_eof = true;
    }
    Ok(())
  }

  /// Returns a slice of all input captured by the reader so far.
  fn captured(&self) -> &[u8] {
    self.prefix.get_ref()
  }

  /// Returns the number of bytes remaining to read from the captured prefix
  /// before consuming more from the source.
  fn captured_unread(&self) -> usize {
    let offset = self.prefix.position() as usize;
    self.prefix.get_ref().len() - offset
  }

  /// Returns true if the latest read from the source indicated an EOF.
  fn is_source_eof(&self) -> bool {
    self.source_eof
  }

  /// Consumes the reader, returning any captured prefix as well as the source.
  fn into_inner(self) -> (Cursor<Vec<u8>>, R) {
    (self.prefix, self.source)
  }
}

impl<R> Read for CaptureReader<R>
where
  R: Read,
{
  fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
    // First, copy as much data as we can from the unread portion of the cursor
    // into the buffer.
    let prefix_size = std::cmp::min(buf.len(), self.captured_unread());
    self.prefix.read_exact(&mut buf[..prefix_size])?;
    if self.captured_unread() > 0 || prefix_size == buf.len() {
      return Ok(prefix_size);
    }

    // Second, fill the rest of the buffer with data from the source, and
    // capture it for ourselves as well.
    //
    // The `read` documentation recommends against us reading from `buf`, but
    // does not prevent it, and does require callers of `read` to assume we
    // might do this. As morally questionable as it is, this approach lets our
    // consumer drive the number and size of reads against the source, making
    // our presence more transparent to both sides. As the smallest consolation,
    // it's worth noting that we only read bytes we know were freshly written,
    // and do not rely on the original contents of `buf` in any way.
    let buf = &mut buf[prefix_size..];
    let source_size = self.source.read(buf)?;
    self.prefix.write_all(&buf[..source_size])?;

    // Finally, mark whether the source has reached EOF. We know that our new
    // `buf` can't be empty as we return early when `prefix_size == buf.len()`,
    // so a 0 byte read can only indicate EOF.
    if source_size == 0 {
      self.source_eof = true;
    }

    Ok(prefix_size + source_size)
  }
}

#[cfg(test)]
mod tests {
  use super::CaptureReader;
  use std::io::{Cursor, Read};

  const DATA: &str = "abcdefghij";
  const HALF: usize = DATA.len() / 2;

  #[test]
  fn rewindable_reader_straight_read() {
    let mut r = CaptureReader::new(Cursor::new(String::from(DATA)));

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
    let mut r = CaptureReader::new(Cursor::new(String::from(DATA)));

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

  #[test]
  fn rewindable_reader_capture_to_end() {
    let mut r = CaptureReader::new(Cursor::new(String::from(DATA)));
    assert!(matches!(r.capture_to_end(), Ok(_)));
    assert_eq!(std::str::from_utf8(r.captured()), Ok(DATA));
    assert!(r.is_source_eof());
  }

  #[test]
  fn rewindable_reader_capture_up_to() {
    let mut r = CaptureReader::new(Cursor::new(String::from(DATA)));

    assert!(matches!(r.capture_to_size(HALF), Ok(_)));

    // We expect the reader to go all the way to its MIN_SIZE for such a small
    // request.
    assert_eq!(std::str::from_utf8(r.captured()), Ok(DATA));
    assert!(r.is_source_eof());
  }
}
