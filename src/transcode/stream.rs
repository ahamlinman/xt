//! Streaming translation between Serde data formats.
//!
//! xt's streaming transcoder is somewhat inspired by the [`serde_transcode`]
//! crate advertised in the Serde documentation. However, its implementation has
//! diverged significantly to enable the preservation of original (de)serializer
//! error values, in contrast to `serde_transcode`'s approach of stringifying
//! errors to meet Serde API requirements.
//!
//! This capability comes at the cost of significant implementation complexity.
//! If it is not an absolute requirement for your use case, you should probably
//! stick with the simpler, more mature, and more popular `serde_transcode`.
//!
//! [`serde_transcode`]: https://github.com/sfackler/serde-transcode

use std::cell::Cell;
use std::error;
use std::fmt;

use serde::de::{self, DeserializeSeed, Deserializer};
use serde::ser::{self, Serialize, SerializeMap, SerializeSeq, Serializer};

/// The message used to generate generic serializer and deserializer errors.
///
/// When transcoding fails due to a serializer error, this message could make
/// its way into the text of the resulting deserializer error, depending on the
/// specific deserializer in use.
const TRANSLATION_FAILED: &str = "translation failed";

/// Transcodes from a Serde `Deserializer` to a Serde `Serializer`.
///
/// The transcoding process forwards values produced by a deserializer directly
/// to a serializer, without collecting the entire output of the deserializer
/// into an intermediate data structure.
///
/// An error in either the serializer or deserializer will immediately halt
/// transcoding. See [`Error`] for information about how to interpret a
/// transcoding error.
///
/// # Caveats
///
/// - The transcoder does not validate the output of the deserializer in any
///   meaningful way. For example, it does not impose recursion limits on the
///   deserialized data, nor does it prevent the deserialization of values that
///   a particular serializer may not support.
///
/// - Transcoding is only possible for self-describing data formats; that is,
///   formats where the types of values can be determined from the serialized
///   representation itself.
///
/// - While the transcoding process itself does not collect the deserializer's
///   entire output, certain (de)serializers may do so as part of their
///   implementation.
///
/// - The current transcoder implementation does not handle all types supported
///   by Serde. It only handles types that a deserializer for a self-describing
///   data format would reasonably be expected to produce (i.e. not
///   Rust-specific types like `Option<T>` or newtype structs).
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
///
/// A transcoding error indicates which side of the transcode originally
/// triggered the failure, and provides access to the original error value(s)
/// generated by the serializer and/or deserializer.
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

/// The internal representation of the side of the transcode operation that
/// originally failed.
///
/// Since the transcoder may need to generate a synthetic serializer error to
/// back out from a deserializer failure, the fact that a serializer error made
/// it up the call stack doesn't mean that the serializer caused the failure.
/// The transcoder explicitly tracks this information so that it can discard
/// these synthetic errors instead of returning them to the user.
#[derive(Clone, Copy)]
enum ErrorSource {
	De,
	Ser,
}

/// The internal state of a single transcoding step, holding data that cannot
/// cross normal Serde API boundaries.
///
/// The transcoding process relies on special implementations of core Serde
/// traits like [`Serialize`] and [`de::Visitor`]. In order to preserve the
/// original values of (de)serializer errors across Serde API boundaries that
/// cannot represent them directly, and to account for the fact that many Serde
/// trait methods can only be called on owned values (i.e. they consume `self`),
/// the transcoder's implementations of Serde trait methods follow a common
/// pattern:
///
/// 1. Take ownership of some "parent" value: either the serializer type to
///    which the next deserialized value should be forwarded, or the
///    deserializer type that will produce the next value.
///
/// 2. Call some method of the parent, possibly with a value that was passed to
///    us as an argument, consuming the parent and producing a [`Result`] of
///    generic value and error types.
///
/// 3. If the parent returned an error that we cannot return directly, capture
///    that original error value (and its source) and instead return whatever
///    error type our method signature requires, either by extracting it from a
///    child `State` or constructing it with a well-known message string (which
///    is the only capability that Serde's error traits require; they cannot be
///    constructed with arbitrary payloads).
///
/// This type is designed to facilitate this pattern, generally as part of a
/// newtype struct that implements a specific Serde trait.
struct State<P, E> {
	// NOTE: Only `Forwarder` actually requires interior mutability for its
	// state (due to `serialize` taking `&self`). However, giving `Forwarder` an
	// individual `Cell` for its full state has a 15% - 25% impact on
	// translation performance for formats that aren't expensive to encode or
	// decode (like MessagePack). As of this writing, I can only guess at the
	// possible optimizations and/or CPU microarchitectural effects that may be
	// at play, since I haven't found a good way to understand this in depth.
	//
	// Serde traits are still implemented on `&mut` references where possible to
	// help mark where mutation is happening, even though `&` references would
	// technically work.
	parent: Cell<Option<P>>,
	error: Cell<Option<E>>,
	source: Cell<ErrorSource>,
}

impl<P, E> State<P, E> {
	/// Creates a new state holding the provided parent value.
	fn new(parent: P) -> State<P, E> {
		State {
			parent: Cell::new(Some(parent)),
			error: Cell::new(None),
			source: Cell::new(ErrorSource::De),
		}
	}

	/// Returns the contained parent value, as long as it was not previously
	/// taken.
	///
	/// # Panics
	///
	/// Panics if the parent value has already been taken from this state.
	fn take_parent(&self) -> P {
		self.parent
			.replace(None)
			.expect("parent already taken from this state")
	}

	/// Captures an error for later extraction, and records the provided source
	/// (serializer or deserializer) as the source of the error.
	fn capture_error(&self, source: ErrorSource, error: E) {
		self.source.set(source);
		self.error.set(Some(error));
	}

	/// Consumes and captures both the error value and error source from another
	/// state.
	fn capture_child_error<C>(&self, child: State<C, E>) {
		self.source.set(child.error_source());
		self.error.set(child.into_error());
	}

	/// Returns the source (serializer or deserializer) of any error associated
	/// with this state.
	///
	/// Note that this returns [`ErrorSource::De`] if no explicit error or
	/// source has been captured. Because the transcoder is fully responsible
	/// for driving the serializer, it will always know when it needs to record
	/// that an error originated there.
	fn error_source(&self) -> ErrorSource {
		self.source.get()
	}

	/// Returns the captured error, if any, consuming the `self` value.
	fn into_error(self) -> Option<E> {
		self.error.into_inner()
	}
}

/// Takes the next value produced by a [`Deserializer`] and forwards it to a
/// [`Serializer`].
///
/// The deserializer's [`Deserializer::deserialize_any`] will drive the
/// transcoding process by calling the [`de::Visitor`] method corresponding to
/// the type of value it sees in its input.
struct Visitor<S: Serializer>(State<S, S::Error>);

impl<S: Serializer> Visitor<S> {
	fn new(ser: S) -> Visitor<S> {
		Visitor(State::new(ser))
	}

	/// Extracts the serializer from the state so it can be used to serialize a
	/// scalar value, then captures and maps any serializer error to a
	/// deserializer error.
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
macro_rules! xt_transcode_impl_scalar_visitors {
	($($name:ident($($arg:ident: $ty:ty)?) => $op:expr;)*) => {
		$(fn $name<E: ::serde::de::Error>(self, $($arg: $ty)?) -> ::std::result::Result<Self::Value, E> {
			self.forward_scalar($op)
		})*
	};
}

impl<'de, S: Serializer> de::Visitor<'de> for &mut Visitor<S> {
	// It's important to note that we do not attempt to implement error
	// preservation by having the visitor return `Result<S::Ok, S::Error>`,
	// which might seem at first like the most obvious way to do things.
	// That approach requires one of two things to happen when the transcoder
	// encounters a serializer error in the middle of a complex value (sequence
	// or map), neither of which is ideal:
	//
	// 1. The visitor can try to return the `Result` immediately, without having
	//    visited the entire map or sequence. My past experience is that this
	//    could violate some deserializers' internal invariants and trigger
	//    panics. Perhaps that's a bug in the deserializer; either way it
	//    doesn't seem worth the risk.
	//
	// 2. To avoid the panics described in point 1, the visitor can "drain" the
	//    rest of a map or sequence from the deserializer before it returns the
	//    final `Result`, e.g. by deserializing into `IgnoredAny`. This works,
	//    but wastes a lot of effort for an operation that we know will fail.
	//
	// Unlike the `Result` approach, the error capturing and mapping approach
	// ensures that a failed transcode follows the expected paths for error
	// handling in both the serializer and deserializer.
	type Value = S::Ok;

	fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
		f.write_str("any supported value")
	}

	xt_transcode_impl_scalar_visitors! {
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

/// A serializable "value" that actually serializes the next value produced by a
/// [`Deserializer`].
///
/// This is required to transcode the individual elements of complex types like
/// sequences and maps, as the serializer traits responsible for these types can
/// only accept values implementing [`Serialize`].
///
/// Unlike most serializable types, a `Forwarder` can only be serialized once,
/// and will panic if serialized more than once.
struct Forwarder<'de, D: Deserializer<'de>>(State<D, D::Error>);

impl<'de, D: Deserializer<'de>> Forwarder<'de, D> {
	fn new(de: D) -> Forwarder<'de, D> {
		Forwarder(State::new(de))
	}

	/// Extracts a serializer type from the state, forwards the deserializer's
	/// next value to it, and captures and maps any serializer error to a
	/// deserializer error.
	///
	/// This helper provides most of the necessary boilerplate to implement the
	/// transcoder's error propagation strategy for the elements of complex
	/// types, each of which is associated with its own set of Serde traits and
	/// collection-specific methods (e.g. to distinguish map keys and values).
	fn serialize_with_seed<S, SErr, F>(
		self,
		seed_state: &mut State<S, SErr>,
		use_serializer: F,
	) -> Result<(), D::Error>
	where
		F: FnOnce(S, &Self) -> Result<(), SErr>,
	{
		let ser = seed_state.take_parent();
		// REMINDER: &self has interior mutability, so serialization may capture
		// an error and source into the state at self.0.
		match use_serializer(ser, &self) {
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

/// Receives the next value of a sequence (from [`de::SeqAccess`]) and forwards
/// it to a sequence serializer (through [`ser::SerializeSeq`]).
struct SeqSeed<'ser, S: SerializeSeq>(State<&'ser mut S, S::Error>);

impl<'ser, S: SerializeSeq> SeqSeed<'ser, S> {
	fn new(ser: &'ser mut S) -> SeqSeed<'ser, S> {
		SeqSeed(State::new(ser))
	}
}

impl<'de, S: SerializeSeq> DeserializeSeed<'de> for &mut SeqSeed<'_, S> {
	type Value = ();

	fn deserialize<D: Deserializer<'de>>(self, de: D) -> Result<(), D::Error> {
		Forwarder::new(de)
			.serialize_with_seed(&mut self.0, |ser, element| ser.serialize_element(element))
	}
}

/// Receives a map key (from [`de::MapAccess`]) and forwards it to a map
/// serializer (through [`ser::SerializeMap`]).
struct KeySeed<'ser, S: SerializeMap>(State<&'ser mut S, S::Error>);

impl<'ser, S: SerializeMap> KeySeed<'ser, S> {
	fn new(ser: &'ser mut S) -> KeySeed<'ser, S> {
		KeySeed(State::new(ser))
	}
}

impl<'de, S: SerializeMap> DeserializeSeed<'de> for &mut KeySeed<'_, S> {
	type Value = ();

	fn deserialize<D: Deserializer<'de>>(self, de: D) -> Result<(), D::Error> {
		Forwarder::new(de).serialize_with_seed(&mut self.0, |ser, key| ser.serialize_key(key))
	}
}

/// Receives a map value (from [`de::MapAccess`]) and forwards it to a map
/// serializer (through [`ser::SerializeMap`]).
struct ValueSeed<'ser, S: SerializeMap>(State<&'ser mut S, S::Error>);

impl<'ser, S: SerializeMap> ValueSeed<'ser, S> {
	fn new(ser: &'ser mut S) -> ValueSeed<'ser, S> {
		ValueSeed(State::new(ser))
	}
}

impl<'de, S: SerializeMap> DeserializeSeed<'de> for &mut ValueSeed<'_, S> {
	type Value = ();

	fn deserialize<D: Deserializer<'de>>(self, de: D) -> Result<(), D::Error> {
		Forwarder::new(de).serialize_with_seed(&mut self.0, |ser, value| ser.serialize_value(value))
	}
}
