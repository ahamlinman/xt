//! Streaming text encoding support for YAML 1.2 streams.

use std::cmp::min;
use std::error::Error;
use std::fmt::{Debug, Display, LowerHex};
use std::io::{self, BufRead, Read, Write};

/// The possible text encodings of a valid YAML 1.2 stream.
pub(super) enum Encoding {
	Utf8,
	Utf16Big,
	Utf32Big,
	Utf16Little,
	Utf32Little,
}

impl Encoding {
	/// The desired length of the prefix for encoding detection.
	pub(super) const DETECT_LEN: usize = 4;

	/// Detects the text encoding of a YAML 1.2 stream based on its leading
	/// bytes.
	///
	/// The detection algorithm is defined in [section 5.2 of the YAML 1.22
	/// specification][spec], and relies on the fact that a valid YAML stream
	/// must begin with either a Unicode byte order mark or an ASCII character.
	/// Detection behavior for non-YAML inputs is not well-defined.
	///
	/// The detector looks at up to 4 bytes of the prefix. If the prefix is less
	/// than 4 bytes and the document is longer than the prefix, the result of
	/// the detection may be incorrect.
	///
	/// [spec]: https://yaml.org/spec/1.2.2/#52-character-encodings
	pub(super) fn detect(prefix: &[u8]) -> Encoding {
		if let Some(Ok(prefix)) = prefix.get(0..4).map(TryInto::<[u8; 4]>::try_into) {
			match prefix {
				[0, 0, 0xFE, 0xFF] | [0, 0, 0, _] => return Encoding::Utf32Big,
				[0xFF, 0xFE, 0, 0] | [_, 0, 0, 0] => return Encoding::Utf32Little,
				_ => {}
			};
		}
		if let Some(Ok(prefix)) = prefix.get(0..2).map(TryInto::<[u8; 2]>::try_into) {
			match prefix {
				[0xFE, 0xFF] | [0, _] => return Encoding::Utf16Big,
				[0xFF, 0xFE] | [_, 0] => return Encoding::Utf16Little,
				_ => {}
			};
		}
		Encoding::Utf8
	}
}

/// Reads a YAML 1.2 stream as UTF-8 regardless of its source encoding.
///
/// Given a UTF-16 or UTF-32 YAML stream, an `Encoder` can transparently
/// re-encode it to UTF-8 and strip any initial byte order mark as it is read
/// from, improving compatibility with parsers that do not accept the full range
/// of supported YAML encodings. Otherwise, an `Encoder` can pass through a
/// UTF-8 stream with little overhead.
pub(super) struct Encoder<R>(EncoderKind<R>)
where
	R: BufRead;

enum EncoderKind<R>
where
	R: BufRead,
{
	Passthrough(R),
	From16(Utf8Encoder<Utf16Decoder<R>>),
	From32(Utf8Encoder<Utf32Decoder<R>>),
}

impl<R> Encoder<R>
where
	R: BufRead,
{
	/// Creates a transcoder using a known source encoding.
	#[allow(clippy::enum_glob_use)]
	pub(super) fn new(reader: R, from: Encoding) -> Self {
		use EncoderKind::*;
		use Encoding::*;
		use Endianness::*;

		Self(match from {
			Utf8 => Passthrough(reader),
			Utf16Big => From16(Utf8Encoder::new(Utf16Decoder::new(reader, Big))),
			Utf32Big => From32(Utf8Encoder::new(Utf32Decoder::new(reader, Big))),
			Utf16Little => From16(Utf8Encoder::new(Utf16Decoder::new(reader, Little))),
			Utf32Little => From32(Utf8Encoder::new(Utf32Decoder::new(reader, Little))),
		})
	}

	/// Creates a transcoder by detecting the source encoding from the first
	/// bytes of the reader.
	///
	/// See [`Encoding::detect`] for details of the detection process. Note that
	/// `from_reader` provides as many prefix bytes to the detector as it needs
	/// for accurate detection.
	pub fn from_reader(mut reader: R) -> io::Result<impl Read> {
		let mut prefix = ArrayBuffer::<{ Encoding::DETECT_LEN }>::new();
		io::copy(
			&mut (&mut reader).take(Encoding::DETECT_LEN as u64),
			&mut prefix,
		)?;
		let encoding = Encoding::detect(prefix.unread());
		Ok(Encoder::new(prefix.chain(reader), encoding))
	}
}

impl<R> Read for Encoder<R>
where
	R: BufRead,
{
	fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
		match &mut self.0 {
			EncoderKind::Passthrough(r) => r.read(buf),
			EncoderKind::From16(r) => r.read(buf),
			EncoderKind::From32(r) => r.read(buf),
		}
	}

	fn read_to_string(&mut self, buf: &mut String) -> io::Result<usize> {
		match &mut self.0 {
			EncoderKind::Passthrough(r) => r.read_to_string(buf),
			EncoderKind::From16(r) => r.read_to_string(buf),
			EncoderKind::From32(r) => r.read_to_string(buf),
		}
	}
}

/// The required size of a buffer large enough to encode any `char` as UTF-8,
/// per [`char::encode_utf8`].
const MAX_UTF8_ENCODED_LEN: usize = 4;

/// A streaming UTF-8 encoder that pairs with [`Utf16Decoder`] or
/// [`Utf32Decoder`].
///
/// If the source document starts with a BOM, the encoder will skip it and
/// reading will begin with the actual text content.
struct Utf8Encoder<S>
where
	S: Iterator<Item = io::Result<char>>,
{
	source: S,
	remainder: ArrayBuffer<MAX_UTF8_ENCODED_LEN>,
	started: bool,
}

impl<S> Utf8Encoder<S>
where
	S: Iterator<Item = io::Result<char>>,
{
	fn new(source: S) -> Self {
		Self {
			source,
			remainder: ArrayBuffer::new(),
			started: false,
		}
	}

	fn next_char(&mut self) -> Option<io::Result<char>> {
		match self.started {
			true => self.source.next(),
			false => {
				self.started = true;
				match self.source.next() {
					Some(Ok(ch)) if ch == '\u{FEFF}' => self.source.next(),
					next => next,
				}
			}
		}
	}
}

impl<S> Read for Utf8Encoder<S>
where
	S: Iterator<Item = io::Result<char>>,
{
	fn read(&mut self, mut buf: &mut [u8]) -> io::Result<usize> {
		let mut written = 0;

		// First, before encoding any new characters, emit the remainder of any
		// character generated by a previous read.
		if !self.remainder.is_empty() {
			let len = self.remainder.read(buf)?;
			buf = &mut buf[len..];
			written += len;
			if !self.remainder.is_empty() {
				return Ok(written);
			}
		}

		// Second, emit as much as we can directly into the destination buffer.
		while buf.len() >= MAX_UTF8_ENCODED_LEN {
			let ch = match self.next_char() {
				Some(Ok(ch)) => ch,
				Some(Err(err)) => return Err(err),
				None => return Ok(written),
			};
			let len = ch.encode_utf8(buf).len();
			buf = &mut buf[len..];
			written += len;
		}

		// Finally, emit as much as we can into the destination buffer's
		// remaining space, storing the remainder of any character that we
		// cannot fully emit at this time.
		while !buf.is_empty() {
			let ch = match self.next_char() {
				Some(Ok(ch)) => ch,
				Some(Err(err)) => return Err(err),
				None => return Ok(written),
			};

			let mut tmp = [0u8; MAX_UTF8_ENCODED_LEN];
			let char_len = ch.encode_utf8(&mut tmp).len();

			let emit_len = min(char_len, buf.len());
			buf[..emit_len].copy_from_slice(&tmp[..emit_len]);
			buf = &mut buf[emit_len..];
			written += emit_len;

			if buf.is_empty() {
				self.remainder.set(&tmp[emit_len..char_len]);
			}
		}

		Ok(written)
	}

	fn read_to_string(&mut self, buf: &mut String) -> io::Result<usize> {
		if !self.remainder.is_empty() {
			return Err(io::Error::new(
				io::ErrorKind::InvalidData,
				"cannot read to string starting from a partial character boundary",
			));
		}

		let mut written = 0;
		while let Some(next) = self.next_char() {
			match next {
				Err(err) if err.kind() == io::ErrorKind::Interrupted => continue,
				Err(err) => return Err(err),
				Ok(ch) => {
					buf.push(ch);
					written += ch.len_utf8();
				}
			}
		}
		Ok(written)
	}
}

/// A streaming UTF-16 decoder.
struct Utf16Decoder<R>
where
	R: BufRead,
{
	source: R,
	pos: u64,
	endianness: Endianness,
	buf: Option<u16>,
}

impl<R> Utf16Decoder<R>
where
	R: BufRead,
{
	fn new(source: R, endianness: Endianness) -> Self {
		Self {
			source,
			pos: 0,
			endianness,
			buf: None,
		}
	}

	fn next_u16(&mut self) -> io::Result<Option<u16>> {
		match self.source.fill_buf() {
			Ok(buf) if buf.is_empty() => return Ok(None),
			Err(err) => return Err(err),
			_ => {}
		};

		let mut next = [0u8; 2];
		self.source.read_exact(&mut next)?;
		self.pos += next.len() as u64;
		Ok(Some(self.endianness.decode_u16(next)))
	}
}

impl<R> Iterator for Utf16Decoder<R>
where
	R: BufRead,
{
	type Item = io::Result<char>;

	fn next(&mut self) -> Option<Self::Item> {
		// This is based on the implementation of `std::char::DecodeUtf16` from
		// the standard library, but is reworked for improved error handling and
		// (hopefully) a bit more readability.

		let pos = self.pos;
		let lead = match self.buf.take() {
			Some(u) => u,
			None => match self.next_u16() {
				Ok(Some(u)) => u,
				Ok(None) => return None,
				Err(err) => return Some(Err(err)),
			},
		};

		if !(0xD800..=0xDFFF).contains(&lead) {
			// SAFETY: This is not a UTF-16 surrogate, which means that the u16
			// code unit directly encodes the desired code point.
			return Some(Ok(unsafe { char::from_u32_unchecked(u32::from(lead)) }));
		}

		if lead >= 0xDC00 {
			// Invalid: a UTF-16 trailing surrogate with no leading surrogate.
			return Some(Err(EncodingError::new(lead, pos).into()));
		}

		let pos = self.pos;
		let trail = match self.next_u16() {
			Ok(Some(u)) => u,
			Ok(None) => return Some(Err(io::ErrorKind::UnexpectedEof.into())),
			Err(err) => return Some(Err(err)),
		};
		if !(0xDC00..=0xDFFF).contains(&trail) {
			// Invalid: we needed a trailing surrogate and didn't get one. We'll
			// try to decode this as a leading code unit on the next iteration.
			self.buf = Some(trail);
			return Some(Err(EncodingError::new(trail, pos).into()));
		}

		// At this point, we are confident that we have valid leading and
		// trailing surrogates, and can decode them into the correct code point.
		let ch = 0x1_0000 + (u32::from(lead - 0xD800) << 10 | u32::from(trail - 0xDC00));
		// SAFETY: We have confirmed that the surrogate pair is valid.
		Some(Ok(unsafe { char::from_u32_unchecked(ch as u32) }))
	}
}

/// A streaming UTF-32 decoder.
struct Utf32Decoder<R>
where
	R: BufRead,
{
	source: R,
	pos: u64,
	endianness: Endianness,
}

impl<R> Utf32Decoder<R>
where
	R: BufRead,
{
	fn new(source: R, endianness: Endianness) -> Self {
		Self {
			source,
			pos: 0,
			endianness,
		}
	}
}

impl<R> Iterator for Utf32Decoder<R>
where
	R: BufRead,
{
	type Item = io::Result<char>;

	fn next(&mut self) -> Option<Self::Item> {
		match self.source.fill_buf() {
			Ok(buf) if buf.is_empty() => return None,
			Err(err) => return Some(Err(err)),
			_ => {}
		};

		let pos = self.pos;
		let mut next = [0u8; 4];
		if let Err(err) = self.source.read_exact(&mut next) {
			return Some(Err(err));
		}
		self.pos += next.len() as u64;

		let unit = self.endianness.decode_u32(next);
		Some(match char::from_u32(unit) {
			Some(ch) => Ok(ch),
			None => Err(EncodingError::new(unit, pos).into()),
		})
	}
}

/// Represents the endianness of UTF-16 or UTF-32 text.
enum Endianness {
	Big,
	Little,
}

impl Endianness {
	fn decode_u16(&self, buf: [u8; 2]) -> u16 {
		match self {
			Endianness::Big => u16::from_be_bytes(buf),
			Endianness::Little => u16::from_le_bytes(buf),
		}
	}

	fn decode_u32(&self, buf: [u8; 4]) -> u32 {
		match self {
			Endianness::Big => u32::from_be_bytes(buf),
			Endianness::Little => u32::from_le_bytes(buf),
		}
	}
}

/// An error in a UTF-16 or UTF-32 stream.
#[derive(Debug)]
struct EncodingError<T>
where
	T: CodeUnit,
{
	unit: T,
	pos: u64,
}

trait CodeUnit: Debug + LowerHex + Send + Sync + 'static {}

impl CodeUnit for u16 {}
impl CodeUnit for u32 {}

impl<T> EncodingError<T>
where
	T: CodeUnit,
{
	const BIT_SIZE: usize = std::mem::size_of::<T>() * 8;

	fn new(unit: T, pos: u64) -> Self {
		Self { unit, pos }
	}
}

impl<T> From<EncodingError<T>> for io::Error
where
	T: CodeUnit,
{
	fn from(err: EncodingError<T>) -> Self {
		io::Error::new(io::ErrorKind::InvalidData, err)
	}
}

impl<T> Error for EncodingError<T> where T: CodeUnit {}

impl<T> Display for EncodingError<T>
where
	T: CodeUnit,
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"invalid or unexpected UTF-{} code unit 0x{:x} at byte {}",
			Self::BIT_SIZE,
			self.unit,
			self.pos,
		)
	}
}

/// A reusable fixed-size buffer with one-way read and write support.
///
/// The array backing an `ArrayBuffer` is logically divided into three
/// contiguous sections:
///
/// - The *read* section, whose contents have been consumed by previous reads.
/// - The *unread* section, which future reads will produce from.
/// - The *unwritten* section, which future writes will populate.
///
/// Writes append to the unread section of the array, shrinking the unwritten
/// section. The space in the read section is never reclaimed automatically for
/// future writes. Instead, an `ArrayBuffer` can be emptied and reinitialized
/// using `set`, optionally with an initial slice of unread bytes.
struct ArrayBuffer<const SIZE: usize> {
	buf: [u8; SIZE],
	pos: usize,
	len: usize,
}

impl<const SIZE: usize> ArrayBuffer<SIZE> {
	/// Returns a new empty buffer.
	fn new() -> Self {
		Self {
			buf: [0u8; SIZE],
			pos: 0,
			len: 0,
		}
	}

	/// Returns the unread portion of the buffer as a slice.
	fn unread(&self) -> &[u8] {
		&self.buf[self.pos..self.len]
	}

	/// Returns whether the buffer is empty; that is, whether it contains no
	/// unread content.
	fn is_empty(&self) -> bool {
		self.unread().is_empty()
	}

	/// Empties and reinitializes the buffer, optionally with an initial slice
	/// of unread bytes.
	///
	/// # Panics
	///
	/// Panics if `buf` is larger than the static size of the buffer.
	fn set(&mut self, buf: &[u8]) {
		let n = buf.len();
		debug_assert!(
			n <= SIZE,
			"called ArrayBuffer::set with a slice of size {} on an ArrayBuffer of size {}",
			n,
			SIZE,
		);
		self.buf[..n].copy_from_slice(buf);
		self.pos = 0;
		self.len = n;
	}
}

impl<const SIZE: usize> Read for ArrayBuffer<SIZE> {
	fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
		let unread = self.unread();
		let n = min(unread.len(), buf.len());
		buf[..n].copy_from_slice(&unread[..n]);
		self.pos += n;
		Ok(n)
	}
}

impl<const SIZE: usize> BufRead for ArrayBuffer<SIZE> {
	fn fill_buf(&mut self) -> io::Result<&[u8]> {
		Ok(self.unread())
	}

	fn consume(&mut self, amt: usize) {
		self.pos += amt;
	}
}

impl<const SIZE: usize> Write for ArrayBuffer<SIZE> {
	fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
		let unwritten = &mut self.buf[self.len..SIZE];
		let n = min(unwritten.len(), buf.len());
		unwritten[..n].copy_from_slice(&buf[..n]);
		self.len += n;
		Ok(n)
	}

	fn flush(&mut self) -> io::Result<()> {
		Ok(())
	}
}
