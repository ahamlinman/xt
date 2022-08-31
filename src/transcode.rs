//! Translation between Serde data formats.
//!
//! xt's transcoder is somewhat inspired by the [`serde_transcode`] crate
//! advertised in the Serde documentation. However, its implementation has
//! diverged significantly to enable the preservation of original (de)serializer
//! error values, in contrast to `serde_transcode`'s approach of stringifying
//! errors to meet Serde API requirements.
//!
//! Some other key differences from `serde_transcode`:
//!
//! - xt's transcoder defines a custom `Error` type that wraps the original
//!   serializer and deserializer error values, and indicates which side of the
//!   transcode initially failed. When the serializer triggers the failure, the
//!   transcoder includes a corresponding deserializer error for additional
//!   context. For example, when xt attempts to transcode a `null` map key in a
//!   YAML file to JSON, and the JSON encoder halts the transcode by refusing to
//!   accept the non-string key, xt will print the line and column of the null
//!   key in the YAML input, as the YAML deserializer's error provides this
//!   information.
//!
//! - `serde_transcode` exposes a `Transcoder` type that implements `Serialize`
//!   for a `Deserializer`, while xt's transcoder only exposes a top-level
//!   `transcode` function. The top-level function enables xt's transcoder to
//!   cleanly expose the richer errors described above, while a public
//!   `Serialize` implementation would force end users to extract these richer
//!   errors through some separate API.
//!
//! - xt does not support transcoding `Option<T>` and newtype struct values, as
//!   no xt input format is expected to produce such values on its own. The
//!   implementation could be extended to support this.
//!
//! Most importantly of all:
//!
//! - xt's transcoder is less mature than `serde_transcode`, and more complex
//!   (read: less maintainable). If the error propagation support isn't an
//!   absolute requirement for your use case, **you should really just use
//!   `serde_transcode`**.
//!
//! [`serde_transcode`]: https://github.com/sfackler/serde-transcode
//!
//! # Implementation Notes
//!
//! Consider the following [`Serialize`] implementation. While it's ridiculously
//! simplified, it exhibits the properties of the [`Serialize`] trait that pose
//! challenges for a Serde transcoder, many of which are shared by other core
//! Serde traits.
//!
//! ```
//! use serde::{Serialize, Serializer};
//!
//! struct FortyTwo;
//!
//! impl Serialize for FortyTwo {
//!     fn serialize<S: Serializer>(&self, ser: S) -> Result<S::Ok, S::Error> {
//!         ser.serialize_u8(42)
//!     }
//! }
//! ```
//!
//! We can see that the intended use of a [`Serializer`] is to call a method
//! corresponding to the Serde type that the data structure maps to, consuming
//! the serializer and returning a result of its own associated types (complex
//! types like sequences and maps have more steps, but the general concept is
//! the same). We can construct our own `S::Error` if necessary through the
//! [`ser::Error::custom`] function, but can only provide a stringifiable
//! message value for context. `serialize` takes `&self`, implying that
//! serialization is not expected to mutate the original value.
//!
//! Now, consider a transcoding implementation of [`Serialize`]:
//!
//! 1. Unlike a typical data structure, it cannot know which `Serializer` method
//!    to call until it receives input from a `Deserializer`. Like `Serializer`,
//!    the methods of a `Deserializer` consume their `self` value, which means
//!    that `serialize` must pull an owned value out of its `&self` parameter.
//!
//! 2. If the deserializer fails, for example due to a syntax error in its
//!    input, `serialize` cannot return that error directly without losing
//!    information.  At best, it can generate a synthetic `S::Error` with the
//!    `custom` constructor.
//!
//! 3. If an `S::Error` bubbles up from nested `serialize` calls, for example
//!    while serializing individual sequence or map elements, the transcoder
//!    doesn't know enough about `S::Error` to distinguish real serializer
//!    errors from synthetic errors representing deserializer failures, which
//!    can be a useful distinction to make.
//!
//! To solve all of these problems, the transcoder's implementations of Serde
//! traits use internal state to transport owned values across normal Serde API
//! boundaries, using [`Cell`] for interior mutability as required. At a high
//! level, each step of the transcode takes a "parent" `Serializer` or
//! `Deserializer` out of the state, consumes it, stashes away any error whose
//! type doesn't match the method's return signature, and produces an error of
//! the correct type by generating one with a boilerplate message or pulling one
//! out of a child state. It also tracks which side of the transcode originally
//! failed so it can hide synthetic errors from the caller.
//!
//! An important property of this approach is that a failed transcode follows
//! standard error handling paths in both the serializer and deserializer, which
//! may be necessary to maintain their internal invariants. An early version of
//! xt's transcoder was built around a [`de::Visitor`] that yielded
//! `Result<S::Ok, S::Error>` rather than `S::Ok` as `serde_transcode` does, and
//! produced panics on certain kinds of transcoding failures with certain
//! serializers. This approach also avoids injecting artificial values into the
//! serializer to help back out from a deserializer failure, which could produce
//! other legitimate errors. For example, if a transcoder encountered an error
//! deserializing a map value and attempted to serialize a unit (null) value to
//! "complete" the entry for the current key, this would trigger a legitimate
//! error in a TOML serializer that would not otherwise have occurred, as TOML
//! does not support null values.

use std::borrow::Cow;
use std::cell::Cell;
use std::error;
use std::fmt;

use serde::de::{self, Deserialize, DeserializeSeed, Deserializer};
use serde::ser::{self, Serialize, SerializeMap, SerializeSeq, Serializer};

/// The message used to generate generic serializer and deserializer errors.
const TRANSLATION_FAILED: &str = "translation failed";

/// Transcodes from a Serde `Deserializer` to a Serde `Serializer`.
///
/// The transcoder forwards the output produced by the deserializer directly to
/// the serializer without collecting it into an intermediate data structure. An
/// error on either side will halt further transcoding.
pub(crate) fn transcode<'de, S, D>(ser: S, de: D) -> Result<S::Ok, Error<S::Error, D::Error>>
where
	S: Serializer,
	D: Deserializer<'de>,
{
	let visitor = Visitor::from(ser);
	match de.deserialize_any(&visitor) {
		Ok(value) => Ok(value),
		Err(de_err) => match visitor.0.error_source() {
			ErrorSource::Ser => Err(Error::Ser(visitor.0.into_error().unwrap(), de_err)),
			ErrorSource::De => Err(Error::De(de_err)),
		},
	}
}

/// Holds an error produced during transcoding.
#[derive(Debug)]
pub(crate) enum Error<S, D> {
	/// The serializer triggered the transcode failure, for example due to an
	/// input value it could not handle. The included deserializer error may
	/// provide useful context, such as the location of the value that the
	/// serializer could not handle.
	Ser(S, D),
	/// The deserializer triggered the transcode failure, for example due to a
	/// syntax error in the input.
	De(D),
}

impl<S, D> fmt::Display for Error<S, D>
where
	S: ser::Error,
	D: de::Error,
{
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Error::Ser(ser_err, de_err) => write!(f, "{}: {}", de_err, ser_err),
			Error::De(de_err) => fmt::Display::fmt(de_err, f),
		}
	}
}

impl<S, D> error::Error for Error<S, D>
where
	S: ser::Error + 'static,
	D: de::Error + 'static,
{
	fn source(&self) -> Option<&(dyn error::Error + 'static)> {
		match self {
			Error::Ser(ser_err, _) => Some(ser_err),
			Error::De(de_err) => Some(de_err),
		}
	}
}

/// The internal state of a single transcoding step, holding data that cannot
/// cross normal Serde API boundaries.
struct State<P, E> {
	parent: Cell<Option<P>>,
	error: Cell<Option<E>>,
	source: Cell<ErrorSource>,
}

/// The side of a transcode operation that originally failed.
///
/// Since the transcoder can generate synthetic serializer errors when the
/// deserializer fails, the mere fact that a serializer error made it up the
/// call stack doesn't mean the serializer originally caused the failure. The
/// transcoder tracks this state separately so it knows when to discard a
/// serializer error instead of returning it.
#[derive(Clone, Copy)]
enum ErrorSource {
	De,
	Ser,
}

impl Default for ErrorSource {
	/// Returns [`ErrorSource::De`].
	///
	/// Since the transcoder drives the serializer, it can always know when to
	/// capture that an error originated from the serializer. So, all errors
	/// with an unknown source must have originated in the deserializer.
	fn default() -> Self {
		ErrorSource::De
	}
}

impl<P, E> State<P, E> {
	fn new(parent: P) -> State<P, E> {
		State {
			parent: Cell::new(Some(parent)),
			error: Cell::new(None),
			source: Cell::new(ErrorSource::De),
		}
	}

	fn take_parent(&self) -> P {
		self.parent
			.replace(None)
			.expect("parent already taken from this state")
	}

	fn capture_error(&self, source: ErrorSource, error: E) {
		self.source.set(source);
		self.error.set(Some(error));
	}

	fn capture_child_error<C>(&self, child: State<C, E>) {
		self.source.set(child.error_source());
		self.error.set(child.into_error());
	}

	fn error_source(&self) -> ErrorSource {
		self.source.get()
	}

	fn into_error(self) -> Option<E> {
		self.error.into_inner()
	}
}

/// Receives the next value from a [`Deserializer`] and forwards it to a
/// [`Serializer`].
struct Visitor<S: Serializer>(State<S, S::Error>);

impl<S: Serializer> From<S> for Visitor<S> {
	fn from(ser: S) -> Self {
		Visitor(State::new(ser))
	}
}

impl<S: Serializer> Visitor<S> {
	fn forward_to_serializer<F, E>(&self, serialize_op: F) -> Result<S::Ok, E>
	where
		F: FnOnce(S) -> Result<S::Ok, S::Error>,
		E: de::Error,
	{
		let ser = self.0.take_parent();
		match serialize_op(ser) {
			Ok(v) => Ok(v),
			Err(ser_err) => {
				self.0.capture_error(ErrorSource::Ser, ser_err);
				Err(de::Error::custom(TRANSLATION_FAILED))
			}
		}
	}
}

impl<'de, S: Serializer> de::Visitor<'de> for &Visitor<S> {
	type Value = S::Ok;

	fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
		f.write_str("any supported value")
	}

	fn visit_unit<E: de::Error>(self) -> Result<Self::Value, E> {
		self.forward_to_serializer(|ser| ser.serialize_unit())
	}

	fn visit_bool<E: de::Error>(self, v: bool) -> Result<Self::Value, E> {
		self.forward_to_serializer(|ser| ser.serialize_bool(v))
	}

	fn visit_i8<E: de::Error>(self, v: i8) -> Result<Self::Value, E> {
		self.forward_to_serializer(|ser| ser.serialize_i8(v))
	}

	fn visit_i16<E: de::Error>(self, v: i16) -> Result<Self::Value, E> {
		self.forward_to_serializer(|ser| ser.serialize_i16(v))
	}

	fn visit_i32<E: de::Error>(self, v: i32) -> Result<Self::Value, E> {
		self.forward_to_serializer(|ser| ser.serialize_i32(v))
	}

	fn visit_i64<E: de::Error>(self, v: i64) -> Result<Self::Value, E> {
		self.forward_to_serializer(|ser| ser.serialize_i64(v))
	}

	fn visit_i128<E: de::Error>(self, v: i128) -> Result<Self::Value, E> {
		self.forward_to_serializer(|ser| ser.serialize_i128(v))
	}

	fn visit_u8<E: de::Error>(self, v: u8) -> Result<Self::Value, E> {
		self.forward_to_serializer(|ser| ser.serialize_u8(v))
	}

	fn visit_u16<E: de::Error>(self, v: u16) -> Result<Self::Value, E> {
		self.forward_to_serializer(|ser| ser.serialize_u16(v))
	}

	fn visit_u32<E: de::Error>(self, v: u32) -> Result<Self::Value, E> {
		self.forward_to_serializer(|ser| ser.serialize_u32(v))
	}

	fn visit_u64<E: de::Error>(self, v: u64) -> Result<Self::Value, E> {
		self.forward_to_serializer(|ser| ser.serialize_u64(v))
	}

	fn visit_u128<E: de::Error>(self, v: u128) -> Result<Self::Value, E> {
		self.forward_to_serializer(|ser| ser.serialize_u128(v))
	}

	fn visit_f32<E: de::Error>(self, v: f32) -> Result<Self::Value, E> {
		self.forward_to_serializer(|ser| ser.serialize_f32(v))
	}

	fn visit_f64<E: de::Error>(self, v: f64) -> Result<Self::Value, E> {
		self.forward_to_serializer(|ser| ser.serialize_f64(v))
	}

	fn visit_char<E: de::Error>(self, v: char) -> Result<Self::Value, E> {
		self.forward_to_serializer(|ser| ser.serialize_char(v))
	}

	fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
		self.forward_to_serializer(|ser| ser.serialize_str(v))
	}

	fn visit_bytes<E: de::Error>(self, v: &[u8]) -> Result<Self::Value, E> {
		self.forward_to_serializer(|ser| ser.serialize_bytes(v))
	}

	fn visit_seq<A: de::SeqAccess<'de>>(self, mut de: A) -> Result<Self::Value, A::Error> {
		let ser = self.0.take_parent();
		let mut ser = match ser.serialize_seq(de.size_hint()) {
			Ok(s) => s,
			Err(ser_err) => {
				self.0.capture_error(ErrorSource::Ser, ser_err);
				return Err(de::Error::custom(TRANSLATION_FAILED));
			}
		};

		loop {
			let seed = SeqSeed::from(&mut ser);
			match de.next_element_seed(&seed) {
				Ok(None) => break,
				Ok(Some(())) => {}
				Err(de_err) => {
					self.0.capture_child_error(seed.0);
					return Err(de_err);
				}
			}
		}

		match ser.end() {
			Ok(v) => Ok(v),
			Err(ser_err) => {
				self.0.capture_error(ErrorSource::Ser, ser_err);
				Err(de::Error::custom(TRANSLATION_FAILED))
			}
		}
	}

	fn visit_map<A: de::MapAccess<'de>>(self, mut de: A) -> Result<Self::Value, A::Error> {
		let ser = self.0.take_parent();
		let mut ser = match ser.serialize_map(de.size_hint()) {
			Ok(m) => m,
			Err(ser_err) => {
				self.0.capture_error(ErrorSource::Ser, ser_err);
				return Err(de::Error::custom(TRANSLATION_FAILED));
			}
		};

		loop {
			let key_seed = KeySeed::from(&mut ser);
			match de.next_key_seed(&key_seed) {
				Ok(None) => break,
				Ok(Some(())) => {}
				Err(de_err) => {
					self.0.capture_child_error(key_seed.0);
					return Err(de_err);
				}
			}

			let value_seed = ValueSeed::from(&mut ser);
			if let Err(de_err) = de.next_value_seed(&value_seed) {
				self.0.capture_child_error(value_seed.0);
				return Err(de_err);
			}
		}

		match ser.end() {
			Ok(v) => Ok(v),
			Err(ser_err) => {
				self.0.capture_error(ErrorSource::Ser, ser_err);
				Err(de::Error::custom(TRANSLATION_FAILED))
			}
		}
	}
}

/// Receives the next value in a sequence from a [`de::SeqAccess`] and forwards
/// it to a [`ser::SerializeSeq`].
struct SeqSeed<'a, S: SerializeSeq>(State<&'a mut S, S::Error>);

impl<'a, S: SerializeSeq> From<&'a mut S> for SeqSeed<'a, S> {
	fn from(ser: &'a mut S) -> Self {
		SeqSeed(State::new(ser))
	}
}

impl<'de, S: SerializeSeq> DeserializeSeed<'de> for &SeqSeed<'_, S> {
	type Value = ();

	fn deserialize<D: Deserializer<'de>>(self, de: D) -> Result<(), D::Error> {
		SerializeNext::forward_from_seed(&self.0, de, |ser, next| ser.serialize_element(next))
	}
}

/// Receives the next map key from a [`de::MapAccess`] and forwards it to a
/// [`ser::SerializeMap`].
struct KeySeed<'a, S: SerializeMap>(State<&'a mut S, S::Error>);

impl<'a, S: SerializeMap> From<&'a mut S> for KeySeed<'a, S> {
	fn from(ser: &'a mut S) -> Self {
		KeySeed(State::new(ser))
	}
}

impl<'de, S: SerializeMap> DeserializeSeed<'de> for &KeySeed<'_, S> {
	type Value = ();

	fn deserialize<D: Deserializer<'de>>(self, de: D) -> Result<(), D::Error> {
		SerializeNext::forward_from_seed(&self.0, de, |ser, next| ser.serialize_key(next))
	}
}

/// Receives the next map value from a [`de::MapAccess`] and forwards it to a
/// [`ser::SerializeMap`].
struct ValueSeed<'a, S: SerializeMap>(State<&'a mut S, S::Error>);

impl<'a, S: SerializeMap> From<&'a mut S> for ValueSeed<'a, S> {
	fn from(ser: &'a mut S) -> Self {
		ValueSeed(State::new(ser))
	}
}

impl<'de, S: SerializeMap> DeserializeSeed<'de> for &ValueSeed<'_, S> {
	type Value = ();

	fn deserialize<D: Deserializer<'de>>(self, de: D) -> Result<(), D::Error> {
		SerializeNext::forward_from_seed(&self.0, de, |ser, next| ser.serialize_value(next))
	}
}

/// A [`Serialize`] implementation that outputs the next value produced by a
/// [`Deserializer`].
struct SerializeNext<'de, D: Deserializer<'de>>(State<D, D::Error>);

impl<'de, D: Deserializer<'de>> SerializeNext<'de, D> {
	fn forward_from_seed<S, E, F>(
		seed_state: &State<S, E>,
		de: D,
		serialize_op: F,
	) -> Result<(), D::Error>
	where
		F: FnOnce(S, &Self) -> Result<(), E>,
	{
		let ser = seed_state.take_parent();
		let next = SerializeNext::from(de);
		match serialize_op(ser, &next) {
			Ok(()) => Ok(()),
			Err(ser_err) => {
				seed_state.capture_error(next.0.error_source(), ser_err);
				match next.0.into_error() {
					Some(de_err) => Err(de_err),
					None => Err(de::Error::custom(TRANSLATION_FAILED)),
				}
			}
		}
	}
}

impl<'de, D: Deserializer<'de>> From<D> for SerializeNext<'de, D> {
	fn from(de: D) -> Self {
		SerializeNext(State::new(de))
	}
}

impl<'de, D: Deserializer<'de>> Serialize for SerializeNext<'de, D> {
	fn serialize<S: Serializer>(&self, ser: S) -> Result<S::Ok, S::Error> {
		let de = self.0.take_parent();
		let visitor = Visitor::from(ser);
		match de.deserialize_any(&visitor) {
			Ok(value) => Ok(value),
			Err(de_err) => {
				self.0.capture_error(visitor.0.error_source(), de_err);
				match visitor.0.into_error() {
					Some(ser_err) => Err(ser_err),
					None => Err(ser::Error::custom(TRANSLATION_FAILED)),
				}
			}
		}
	}
}

/// A deserialized value referencing borrowed data.
///
/// `Value` supports cases where an input format does not provide access to a
/// [`Deserializer`] for direct transcoding, and instead requires deserializing
/// into an in-memory value.
///
/// Unlike most "Serde value" types, `Value` is explicitly optimized for use in
/// transcoding. It uses zero-copy deserialization for byte sequences and
/// strings where possible, which limits the lifetime of the value and the types
/// of inputs it can deserialize from. It represents maps as `Vec`s of key-value
/// pairs, which preserves ordering but does not allow random access to entries.
pub(crate) enum Value<'a> {
	Unit,
	Bool(bool),
	I8(i8),
	I16(i16),
	I32(i32),
	I64(i64),
	I128(i128),
	U8(u8),
	U16(u16),
	U32(u32),
	U64(u64),
	U128(u128),
	F32(f32),
	F64(f64),
	Char(char),
	String(Cow<'a, str>),
	Bytes(Cow<'a, [u8]>),
	Seq(Vec<Value<'a>>),
	Map(Vec<(Value<'a>, Value<'a>)>),
}

impl Serialize for Value<'_> {
	fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		match self {
			Value::Unit => s.serialize_unit(),
			Value::Bool(b) => s.serialize_bool(*b),
			Value::I8(n) => s.serialize_i8(*n),
			Value::I16(n) => s.serialize_i16(*n),
			Value::I32(n) => s.serialize_i32(*n),
			Value::I64(n) => s.serialize_i64(*n),
			Value::I128(n) => s.serialize_i128(*n),
			Value::U8(n) => s.serialize_u8(*n),
			Value::U16(n) => s.serialize_u16(*n),
			Value::U32(n) => s.serialize_u32(*n),
			Value::U64(n) => s.serialize_u64(*n),
			Value::U128(n) => s.serialize_u128(*n),
			Value::F32(f) => s.serialize_f32(*f),
			Value::F64(f) => s.serialize_f64(*f),
			Value::Char(c) => s.serialize_char(*c),
			Value::String(v) => v.serialize(s),
			Value::Bytes(v) => v.serialize(s),
			Value::Seq(v) => v.serialize(s),
			Value::Map(m) => {
				let mut s = s.serialize_map(Some(m.len()))?;
				for (k, v) in m {
					s.serialize_entry(k, v)?;
				}
				s.end()
			}
		}
	}
}

/// Implements [`de::Visitor`] methods that simply shove values into [`Value`]s.
///
/// This macro is non-hygienic, and not intended for use outside of this module.
macro_rules! xt_transcode_impl_value_visitors {
	($($name:ident($($arg:ident: $ty:ty)?) => $result:expr;)*) => {
		$(fn $name<E: de::Error>(self, $($arg: $ty)?) -> Result<Self::Value, E> {
			Ok($result)
		})*
	};
}

impl<'de: 'a, 'a> Deserialize<'de> for Value<'a> {
	fn deserialize<D>(d: D) -> Result<Self, D::Error>
	where
		D: Deserializer<'de>,
	{
		struct Visitor;

		impl<'a> de::Visitor<'a> for Visitor {
			type Value = Value<'a>;

			fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
				f.write_str("any supported value")
			}

			xt_transcode_impl_value_visitors! {
				visit_unit() => Value::Unit;

				visit_bool(v: bool) => Value::Bool(v);

				visit_i8(v: i8) => Value::I8(v);
				visit_i16(v: i16) => Value::I16(v);
				visit_i32(v: i32) => Value::I32(v);
				visit_i64(v: i64) => Value::I64(v);
				visit_i128(v: i128) => Value::I128(v);

				visit_u8(v: u8) => Value::U8(v);
				visit_u16(v: u16) => Value::U16(v);
				visit_u32(v: u32) => Value::U32(v);
				visit_u64(v: u64) => Value::U64(v);
				visit_u128(v: u128) => Value::U128(v);

				visit_f32(v: f32) => Value::F32(v);
				visit_f64(v: f64) => Value::F64(v);

				visit_char(v: char) => Value::Char(v);

				visit_borrowed_str(v: &'a str) => Value::String(Cow::Borrowed(v));
				visit_str(v: &str) => Value::String(Cow::Owned(v.to_owned()));
				visit_string(v: String) => Value::String(Cow::Owned(v));

				visit_borrowed_bytes(v: &'a [u8]) => Value::Bytes(Cow::Borrowed(v));
				visit_bytes(v: &[u8]) => Value::Bytes(Cow::Owned(v.to_owned()));
				visit_byte_buf(v: Vec<u8>) => Value::Bytes(Cow::Owned(v));
			}

			fn visit_seq<A: de::SeqAccess<'a>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
				let mut vec = Vec::with_capacity(seq.size_hint().unwrap_or(0));
				while let Some(e) = seq.next_element()? {
					vec.push(e);
				}
				Ok(Value::Seq(vec))
			}

			fn visit_map<A: de::MapAccess<'a>>(self, mut map: A) -> Result<Self::Value, A::Error> {
				let mut vec = Vec::with_capacity(map.size_hint().unwrap_or(0));
				while let Some(entry) = map.next_entry()? {
					vec.push(entry);
				}
				Ok(Value::Map(vec))
			}
		}

		d.deserialize_any(Visitor)
	}
}
