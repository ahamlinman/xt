//! Streaming translation between Serde data formats.
//!
//! xt's streaming transcoder is somewhat inspired by the [`serde_transcode`]
//! crate advertised in the Serde documentation. However, its implementation has
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
//! produced panics on certain kinds of transcoding failures with a particular
//! deserializer. This approach also avoids injecting artificial values into the
//! serializer to help back out from a deserializer failure, which could produce
//! other legitimate errors. For example, if a transcoder encountered an error
//! deserializing a map value and attempted to serialize a unit (null) value to
//! "complete" the entry for the current key, this would trigger a legitimate
//! error in a TOML serializer that would not otherwise have occurred, as TOML
//! does not support null values.

use std::cell::Cell;
use std::error;
use std::fmt;

use serde::de::{self, DeserializeSeed, Deserializer};
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
	let mut visitor = Visitor::new(ser);
	match de.deserialize_any(&mut visitor) {
		Ok(value) => Ok(value),
		Err(de_err) => match visitor.0.error_source() {
			ErrorSource::Ser => Err(Error::Ser(visitor.0.into_error().unwrap(), de_err)),
			ErrorSource::De => Err(Error::De(de_err)),
		},
	}
}

/// Holds an error produced during transcoding.
#[derive(Debug)]
pub(crate) enum Error<S, D>
where
	S: ser::Error,
	D: de::Error,
{
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
			Error::Ser(ser_err, de_err) => write!(f, "{de_err}: {ser_err}"),
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
	// NOTE: Only `SerializeNext` actually requires interior mutability for its
	// state. However, giving `SerializeNext` an individual `Cell` for the full
	// state has a 15% - 25% impact on translation performance for formats that
	// aren't expensive to encode or decode (like MessagePack). Sadly, I don't
	// understand why this is.
	//
	// Serde traits are still implemented on `&mut` references where possible to
	// clearly mark where mutation is happening, even though `&` references
	// would technically work.
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
	/// Since the transcoder drives the serializer, it always knows when to
	/// capture that an error originated there. Therefore, all errors with an
	/// unknown source must have originated in the deserializer.
	fn default() -> ErrorSource {
		ErrorSource::De
	}
}

impl<P, E> State<P, E> {
	fn new(parent: P) -> State<P, E> {
		State {
			parent: Cell::new(Some(parent)),
			error: Cell::new(None),
			source: Cell::new(ErrorSource::default()),
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

/// A [`de::Visitor`] that receives the next value from a [`Deserializer`] and
/// forwards it to a [`Serializer`].
struct Visitor<S: Serializer>(State<S, S::Error>);

impl<S: Serializer> Visitor<S> {
	// Creates a new visitor that forwards a deserialized value to `ser`.
	fn new(ser: S) -> Visitor<S> {
		Visitor(State::new(ser))
	}

	/// Extracts the serializer from the state so it can be used to serialize a
	/// scalar value, then captures and maps any serializer error.
	///
	/// The only meaningful difference between most of the visitor's methods is
	/// that they each call a specific [`Serializer`] method depending on the
	/// type of value the deserializer gave us. This helper provides all of the
	/// necessary boilerplate to implement the transcoder's error propagation
	/// strategy, without encoding everything into a macro body that tools like
	/// rust-analyzer can't meaningfully inspect.
	fn forward_scalar<F, E>(&mut self, use_serializer: F) -> Result<S::Ok, E>
	where
		F: FnOnce(S) -> Result<S::Ok, S::Error>,
		E: de::Error,
	{
		let ser = self.0.take_parent();
		match use_serializer(ser) {
			Ok(value) => Ok(value),
			Err(ser_err) => {
				self.0.capture_error(ErrorSource::Ser, ser_err);
				Err(de::Error::custom(TRANSLATION_FAILED))
			}
		}
	}
}

/// Implements [`de::Visitor`] methods that simply forward scalar values to the
/// appropriate method of a [`Serializer`].
///
/// As these methods in the transcoder's visitor have nearly identical
/// implementations, this eliminates a significant amount of signature-related
/// boilerplate and makes it easier to focus on the actual mappings.
macro_rules! xt_transcode_impl_forward_visitors {
	($($name:ident($($arg:ident: $ty:ty)?) => $op:expr;)*) => {
		$(fn $name<E: ::serde::de::Error>(self, $($arg: $ty)?) -> ::std::result::Result<Self::Value, E> {
			self.forward_scalar($op)
		})*
	};
}

impl<'de, S: Serializer> de::Visitor<'de> for &mut Visitor<S> {
	type Value = S::Ok;

	fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
		f.write_str("any supported value")
	}

	xt_transcode_impl_forward_visitors! {
		visit_unit() => |ser| ser.serialize_unit();

		visit_bool(v: bool) => |ser| ser.serialize_bool(v);

		visit_i8(v: i8) => |ser| ser.serialize_i8(v);
		visit_i16(v: i16) => |ser| ser.serialize_i16(v);
		visit_i32(v: i32) => |ser| ser.serialize_i32(v);
		visit_i64(v: i64) => |ser| ser.serialize_i64(v);
		visit_i128(v: i128) => |ser| ser.serialize_i128(v);

		visit_u8(v: u8) => |ser| ser.serialize_u8(v);
		visit_u16(v: u16) => |ser| ser.serialize_u16(v);
		visit_u32(v: u32) => |ser| ser.serialize_u32(v);
		visit_u64(v: u64) => |ser| ser.serialize_u64(v);
		visit_u128(v: u128) => |ser| ser.serialize_u128(v);

		visit_f32(v: f32) => |ser| ser.serialize_f32(v);
		visit_f64(v: f64) => |ser| ser.serialize_f64(v);

		visit_char(v: char) => |ser| ser.serialize_char(v);
		visit_str(v: &str) => |ser| ser.serialize_str(v);
		visit_bytes(v: &[u8]) => |ser| ser.serialize_bytes(v);
	}

	fn visit_seq<A: de::SeqAccess<'de>>(self, mut de: A) -> Result<Self::Value, A::Error> {
		let ser = self.0.take_parent();
		let mut seq = match ser.serialize_seq(de.size_hint()) {
			Ok(seq) => seq,
			Err(ser_err) => {
				self.0.capture_error(ErrorSource::Ser, ser_err);
				return Err(de::Error::custom(TRANSLATION_FAILED));
			}
		};

		loop {
			let mut seed = SeqSeed::new(&mut seq);
			match de.next_element_seed(&mut seed) {
				Ok(None) => break,
				Ok(Some(())) => {}
				Err(de_err) => {
					self.0.capture_child_error(seed.0);
					return Err(de_err);
				}
			}
		}

		match seq.end() {
			Ok(value) => Ok(value),
			Err(ser_err) => {
				self.0.capture_error(ErrorSource::Ser, ser_err);
				Err(de::Error::custom(TRANSLATION_FAILED))
			}
		}
	}

	fn visit_map<A: de::MapAccess<'de>>(self, mut de: A) -> Result<Self::Value, A::Error> {
		let ser = self.0.take_parent();
		let mut map = match ser.serialize_map(de.size_hint()) {
			Ok(map) => map,
			Err(ser_err) => {
				self.0.capture_error(ErrorSource::Ser, ser_err);
				return Err(de::Error::custom(TRANSLATION_FAILED));
			}
		};

		loop {
			let mut key_seed = KeySeed::new(&mut map);
			match de.next_key_seed(&mut key_seed) {
				Ok(None) => break,
				Ok(Some(())) => {}
				Err(de_err) => {
					self.0.capture_child_error(key_seed.0);
					return Err(de_err);
				}
			}

			let mut value_seed = ValueSeed::new(&mut map);
			if let Err(de_err) = de.next_value_seed(&mut value_seed) {
				self.0.capture_child_error(value_seed.0);
				return Err(de_err);
			}
		}

		match map.end() {
			Ok(value) => Ok(value),
			Err(ser_err) => {
				self.0.capture_error(ErrorSource::Ser, ser_err);
				Err(de::Error::custom(TRANSLATION_FAILED))
			}
		}
	}
}

/// A [`Serialize`] implementation that outputs the next value produced by a
/// [`Deserializer`].
struct Forwarder<'de, D: Deserializer<'de>>(State<D, D::Error>);

impl<'de, D: Deserializer<'de>> Forwarder<'de, D> {
	fn from(de: D) -> Forwarder<'de, D> {
		Forwarder(State::new(de))
	}

	fn forward_to_seed<S, SErr, F>(
		self,
		seed_state: &mut State<S, SErr>,
		op: F,
	) -> Result<(), D::Error>
	where
		F: FnOnce(S, &Self) -> Result<(), SErr>,
	{
		let ser = seed_state.take_parent();
		match op(ser, &self) {
			Ok(()) => Ok(()),
			Err(ser_err) => {
				seed_state.capture_error(self.0.error_source(), ser_err);
				match self.0.into_error() {
					Some(de_err) => Err(de_err),
					None => Err(de::Error::custom(TRANSLATION_FAILED)),
				}
			}
		}
	}
}

impl<'de, D: Deserializer<'de>> Serialize for Forwarder<'de, D> {
	fn serialize<S: Serializer>(&self, ser: S) -> Result<S::Ok, S::Error> {
		let mut visitor = Visitor::new(ser);
		let de = self.0.take_parent();
		match de.deserialize_any(&mut visitor) {
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

/// Receives the next value in a sequence from a [`de::SeqAccess`] and forwards
/// it to a [`ser::SerializeSeq`].
struct SeqSeed<'a, S: SerializeSeq>(State<&'a mut S, S::Error>);

impl<'a, S: SerializeSeq> SeqSeed<'a, S> {
	fn new(ser: &'a mut S) -> SeqSeed<'a, S> {
		SeqSeed(State::new(ser))
	}
}

impl<'de, S: SerializeSeq> DeserializeSeed<'de> for &mut SeqSeed<'_, S> {
	type Value = ();

	fn deserialize<D: Deserializer<'de>>(self, de: D) -> Result<(), D::Error> {
		Forwarder::from(de)
			.forward_to_seed(&mut self.0, |ser, element| ser.serialize_element(element))
	}
}

/// Receives the next map key from a [`de::MapAccess`] and forwards it to a
/// [`ser::SerializeMap`].
struct KeySeed<'a, S: SerializeMap>(State<&'a mut S, S::Error>);

impl<'a, S: SerializeMap> KeySeed<'a, S> {
	fn new(ser: &'a mut S) -> KeySeed<'a, S> {
		KeySeed(State::new(ser))
	}
}

impl<'de, S: SerializeMap> DeserializeSeed<'de> for &mut KeySeed<'_, S> {
	type Value = ();

	fn deserialize<D: Deserializer<'de>>(self, de: D) -> Result<(), D::Error> {
		Forwarder::from(de).forward_to_seed(&mut self.0, |ser, key| ser.serialize_key(key))
	}
}

/// Receives the next map value from a [`de::MapAccess`] and forwards it to a
/// [`ser::SerializeMap`].
struct ValueSeed<'a, S: SerializeMap>(State<&'a mut S, S::Error>);

impl<'a, S: SerializeMap> ValueSeed<'a, S> {
	fn new(ser: &'a mut S) -> ValueSeed<'a, S> {
		ValueSeed(State::new(ser))
	}
}

impl<'de, S: SerializeMap> DeserializeSeed<'de> for &mut ValueSeed<'_, S> {
	type Value = ();

	fn deserialize<D: Deserializer<'de>>(self, de: D) -> Result<(), D::Error> {
		Forwarder::from(de).forward_to_seed(&mut self.0, |ser, value| ser.serialize_value(value))
	}
}
