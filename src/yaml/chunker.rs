//! Support for splitting YAML 1.2 streams into their constituent documents.
//!
//! This is an awful hack to provide some level of streaming input support atop
//! `serde_yaml`, which as of this writing requires buffering all input before
//! parsing it (the convenience methods that parse from readers simply do this
//! buffering for you). Using the same underlying parser as `serde_yaml` (a Rust
//! translation of the venerable [libyaml][libyaml]), a [`Chunker`] iterates
//! over the documents in a YAML stream as `String`s, which can be provided one
//! by one to `serde_yaml` for actual deserialization.
//!
//! I sincerely hope that I will someday have the time and energy to implement
//! true streaming support in `serde_yaml` itself (unless, of course, someone
//! beats me to it), and that this implementation will serve as a stepping stone
//! toward that goal.
//!
//! [libyaml]: https://pyyaml.org/wiki/LibYAML

use std::io::{self, Read};
use std::mem;

use super::parser::{
	Parser, YAML_DOCUMENT_END_EVENT, YAML_DOCUMENT_START_EVENT, YAML_MAPPING_START_EVENT,
	YAML_SCALAR_EVENT, YAML_SEQUENCE_START_EVENT, YAML_STREAM_END_EVENT,
};

/// An iterator over individual raw documents in a UTF-8-encoded YAML stream.
pub(super) struct Chunker<R>
where
	R: Read,
{
	parser: Parser<ChunkReader<R>>,
	last_document: Option<Document>,
	current_document_kind: Option<DocumentKind>,
	stream_ended: bool,
}

impl<R> Chunker<R>
where
	R: Read,
{
	/// Creates a new chunker for the YAML stream produced by the reader.
	///
	/// YAML 1.2 allows several different text encodings for YAML streams, as
	/// well as the presence of byte order marks at the start of the stream or
	/// individual documents. However, `Chunker` requires a UTF-8 stream without
	/// BOMs. Consider using the [`encoding`](super::encoding) module to
	/// re-encode non-UTF-8 streams.
	pub(super) fn new(reader: R) -> Self {
		Self {
			parser: Parser::new(ChunkReader::new(reader)),
			last_document: None,
			current_document_kind: None,
			stream_ended: false,
		}
	}
}

impl<R> Iterator for Chunker<R>
where
	R: Read,
{
	type Item = io::Result<Document>;

	fn next(&mut self) -> Option<Self::Item> {
		if self.stream_ended {
			return None;
		}

		loop {
			let event = match self.parser.parse() {
				Ok(event) => event,
				Err(err) => return Some(Err(io::Error::new(io::ErrorKind::InvalidData, err))),
			};

			// Note that while we chunk on DOCUMENT_END events, we don't emit
			// the chunk until the next DOCUMENT_START or STREAM_END. The parser
			// sometimes sees valid documents in non-YAML inputs, and only fails
			// when it looks for the start of the next document. This is bad
			// when the chunker's output determines whether an arbitrary input
			// is valid YAML (e.g. xt's format detection).
			match event.event_type() {
				YAML_DOCUMENT_START_EVENT => {
					let offset = event.start_offset();
					self.parser.reader_mut().trim_to_offset(offset);
					self.current_document_kind = None;
					if let Some(doc) = self.last_document.take() {
						return Some(Ok(doc));
					}
				}
				YAML_SCALAR_EVENT => {
					self.current_document_kind
						.get_or_insert(DocumentKind::Scalar);
				}
				YAML_SEQUENCE_START_EVENT | YAML_MAPPING_START_EVENT => {
					self.current_document_kind
						.get_or_insert(DocumentKind::Collection);
				}
				YAML_DOCUMENT_END_EVENT => {
					let chunk = self.parser.reader_mut().take_to_offset(event.end_offset());
					self.last_document = Some(Document {
						content: String::from_utf8(chunk).unwrap(),
						kind: self.current_document_kind.take().unwrap(),
					});
				}
				YAML_STREAM_END_EVENT => {
					self.stream_ended = true;
					return self.last_document.take().map(Ok);
				}
				_ => {}
			};
		}
	}
}

/// A UTF-8 encoded YAML document.
pub(super) struct Document {
	content: String,
	kind: DocumentKind,
}

/// The type of content contained in a YAML document.
pub(super) enum DocumentKind {
	Scalar,
	Collection,
}

impl Document {
	/// Returns the original text of the document.
	pub(super) fn content(&self) -> &str {
		&self.content
	}

	/// Returns true if the content of the document is a scalar rather than a
	/// collection (sequence or mapping).
	pub(super) fn is_scalar(&self) -> bool {
		matches!(self.kind, DocumentKind::Scalar)
	}
}

/// A reader that captures bytes read from a source and provides them in chunks.
struct ChunkReader<R>
where
	R: Read,
{
	reader: R,
	captured: Vec<u8>,
	captured_start_offset: u64,
}

impl<R> ChunkReader<R>
where
	R: Read,
{
	fn new(reader: R) -> Self {
		Self {
			reader,
			captured: vec![],
			captured_start_offset: 0,
		}
	}

	/// Trims from the start of the capture buffer so the next chunk will begin
	/// at the specified reader offset.
	fn trim_to_offset(&mut self, offset: u64) {
		let trim_len = usize::try_from(offset - self.captured_start_offset).unwrap();
		self.captured_start_offset = offset;
		self.captured.drain(..trim_len);
	}

	/// Takes the chunk from the start of the capture buffer up to the specified
	/// reader offset, leaving bytes beyond the offset in the capture buffer.
	fn take_to_offset(&mut self, offset: u64) -> Vec<u8> {
		let take_len = usize::try_from(offset - self.captured_start_offset).unwrap();
		let tail = self.captured.split_off(take_len);
		self.captured_start_offset = offset;
		mem::replace(&mut self.captured, tail)
	}
}

impl<R> Read for ChunkReader<R>
where
	R: Read,
{
	fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
		// While the read documentation recommends against reading from buf, it
		// does not prevent it, and does require callers of read to assume we
		// might do this. As consolation, note that we only read back bytes that
		// we know were freshly written, unless of course the source is broken
		// and lies about how many bytes it read.
		let len = self.reader.read(buf)?;
		self.captured.extend_from_slice(&buf[..len]);
		Ok(len)
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn chunker_normal_usage() {
		const INPUT: &str = r#"---
test: true
---
12345
---
[list, of strings]
"#;

		let chunker = Chunker::new(INPUT.as_bytes());
		let docs = chunker.collect::<Result<Vec<_>, io::Error>>().unwrap();

		let contents = docs.iter().map(|doc| doc.content()).collect::<Vec<_>>();
		assert_eq!(
			&contents,
			&[
				"---\ntest: true\n",
				"---\n12345\n",
				"---\n[list, of strings]\n",
			]
		);

		let scalars = docs.iter().map(|doc| doc.is_scalar()).collect::<Vec<_>>();
		assert_eq!(&scalars, &[false, true, false]);
	}

	#[test]
	#[should_panic]
	fn chunker_misbehaving_reader() {
		let chunker = Chunker::new(MisbehavingReader("---\nevil: true".as_bytes()));
		let _ = chunker.collect::<Vec<_>>();
	}

	/// A reader that always reports having read 1 byte more than the length of
	/// the buffer provided to [`read`](Read::read), regardless of the actual
	/// size of the underlying read.
	struct MisbehavingReader<R: Read>(R);

	impl<R: Read> Read for MisbehavingReader<R> {
		fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
			self.0.read(buf).and(Ok(buf.len() + 1))
		}
	}
}
