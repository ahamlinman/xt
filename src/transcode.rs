//! Functions and types to support translation between Serde data formats.
//!
//! xt's transcoder is somewhat inspired by the [`serde_transcode`] crate
//! advertised in the Serde documentation. However, its implementation has
//! diverged significantly to enable the preservation of original (de)serializer
//! error values, in contrast to `serde_transcode`'s approach of stringifying
//! errors to meet Serde API requirements.
//!
//! # Fundamentals of Transcoding
//!
//! At a fundamental level, transcoding is similar to normal deserialization.
//! Where typical Serde usage would deserialize an input into an in-memory data
//! structure, a Serde transcoder seeks to "deserialize" that input into the
//! abstract result of serializing that data as some other format, including
//! side effects like writing the serialized output to a stream.
//!
//! To summarize the process: a Serde [`Deserializer`] parses some input to find
//! the next useful value, and calls a Serde [`Visitor`][`de::Visitor`] method
//! corresponding to the type of that value. For simple types like numbers and
//! strings, the visitor will receive the value directly.  For complex types
//! like sequences and maps, the deserializer will provide an "accessor" that
//! allows the visitor to deserialize (with another visitor) each element in
//! turn. A typical visitor, of the kind that `serde_derive` might generate,
//! would construct and return some kind of value using these inputs. A
//! transcoding visitor will instead invoke the Serde [`Serializer`] methods
//! corresponding to those inputs.
//!
//! # Implementation Overview
//!
//! The transcoder's implementations of Serde traits are relatively unique, as
//! they often need to transfer owned values across Serde API boundaries that
//! aren't designed to handle them directly.
//!
//! For example, the transcoder must implement Serde's [`Serialize`] trait to
//! serialize the elements of sequences and maps:
//!
//! ```ignore
//! pub trait Serialize {
//!     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//!     where
//!         S: Serializer;
//! }
//! ```
//!
//! A typical `serialize` body will consume the `serializer` by calling one of
//! its methods to emit a value, and directly return the `Result` produced by
//! that call. For example, Serde provides the following for the basic `bool`
//! type:
//!
//! ```ignore
//! impl Serialize for bool {
//!     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//!     where
//!         S: Serializer,
//!     {
//!         serializer.serialize_bool(*self)
//!     }
//! }
//! ```
//!
//! This pattern isn't too hard for a typical data structure to implement.
//! However, the transcoder faces a few challenges:
//!
//! 1. Unlike with a typical data structure, `serialize` can't know which
//!    `serializer` method to call until it asks the deserializer to drive a new
//!    visitor. This consumes the deserializer, which means that `serialize`
//!    will need to safely take ownership of it from the provided `&self`.
//!
//! 2. If the deserializer fails, for example due to a syntax error in the
//!    input, `serialize` can't return that original error directly. It must
//!    return an `S::Error`, which it can only construct with a string-like
//!    value via the [`ser::Error::custom`] function.
//!
//! 3. When `serialize` returns an error, its caller within the transcoder does
//!    not know enough about `S::Error` to distinguish a legitimate serializer
//!    error from a `custom` error generated by the transcoder itself. Without
//!    this information, the transcoder cannot reliably report which side of a
//!    transcode operation caused it to fail.
//!
//! In general, all of the transcoder's trait implementations face similar
//! challenges, with the details varying based on which part of the process
//! they're involved with. As such, these implementations all center around a
//! common [`Transcoder`] type. A `Transcoder` initially owns an unused parent
//! (de)serializer for a trait method to take, provides storage for any error
//! value that the method can't return directly, and tracks the original source
//! of an error to hide synthetic errors from the transcoder's final result.
//!
//! A typical implementation of a Serde trait method on `Transcoder` will:
//!
//! 1. Take ownership of the parent (de)serializer held by the transcoder.
//!    Subsequent attempts to do this will panic.
//!
//! 2. Call the appropriate (de)serializer method for the next step of the
//!    transcode. It might just need to serialize a simple value, or it might
//!    need to construct a new "inner" transcoder to allow the callee to invoke
//!    other Serde trait methods.
//!
//! 3. Capture any state from that call that the method can't return directly,
//!    including the call's true error value and the appropriate error source
//!    information.
//!
//! 4. Return an error of the method's return type, either by constructing one
//!    with a generic message or extracting one from an inner transcoder.
//!
//! This method of error propagation ensures that a failed transcode follows the
//! same error paths that the serializer and deserializer would usually traverse
//! when aborting a (de)serialize operation in the middle of a value. This is
//! important for two reasons:
//!
//! 1. Attempts to inject artificial success into one side of a failed transcode
//!    can generate errors of their own. For example, consider a transcoder that
//!    successfully transcodes a map key, encounters a deserializer error, and
//!    attempts to halt the translation of the map by serializing a unit (null)
//!    value. This transcoder would generate a legitimate error in a TOML
//!    serializer that would not otherwise have occurred, as the TOML format
//!    does not support null values. In general, it is safest to only generate
//!    serializer output in direct response to deserializer input.
//!
//! 2. Attempts to break early from sequence and map deserialization without
//!    reporting an error can violate internal deserializer invariants and
//!    trigger panics. Early implementations of xt's transcoder that implemented
//!    a `Visitor` yielding `Result<S::Ok, S::Error>` rather than `S::Ok`
//!    exhibited this behavior, particularly with `serde_yaml`.
//!
//! # Other Differences From `serde_transcode`
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
//!   errors in similar fashion to the transcoder's internals.
//!
//! - xt does not support transcoding `Option<T>` and newtype struct values, as
//!   no xt input format is expected to produce such values on its own. The
//!   implementation could be extended to support this.
//!
//! Most importantly of all:
//!
//! - xt's transcoder is far less mature than `serde_transcode`, and far more
//!   complex (read: less maintainable). If the error propagation support isn't
//!   an absolute requirement for your use case, **you should really just use
//!   `serde_transcode`**.
//!
//! [`serde_transcode`]: https://github.com/sfackler/serde-transcode

use std::borrow::Cow;
use std::cell::Cell;
use std::fmt;

use serde::{
  de::{self, Deserialize, DeserializeSeed, Deserializer},
  ser::{self, Serialize, SerializeMap, SerializeSeq, Serializer},
};

/// The message used to generate generic serializer and deserializer errors.
const TRANSLATION_FAILED: &str = "translation failed";

/// Transcodes from a Serde `Deserializer` to a Serde `Serializer`.
///
/// The transcoder forwards the output produced by the deserializer directly to
/// the serializer without collecting it into an intermediate data structure. An
/// error on either side will halt further transcoding.
pub(crate) fn transcode<'de, S, D>(s: S, d: D) -> Result<S::Ok, Error<S::Error, D::Error>>
where
  S: Serializer,
  D: Deserializer<'de>,
{
  let visitor = Transcoder::new(s);
  match d.deserialize_any(&visitor) {
    Ok(value) => Ok(value),
    Err(derr) => match visitor.error_source() {
      ErrorSource::Ser => Err(Error::Ser(visitor.into_error().unwrap(), derr)),
      ErrorSource::De => Err(Error::De(derr)),
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
      Error::Ser(serr, derr) => write!(f, "{}: {}", derr, serr),
      Error::De(derr) => fmt::Display::fmt(derr, f),
    }
  }
}

impl<S, D> std::error::Error for Error<S, D>
where
  S: ser::Error + 'static,
  D: de::Error + 'static,
{
  fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
    match self {
      Error::Ser(serr, _) => Some(serr),
      Error::De(derr) => Some(derr),
    }
  }
}

/// A generic helper representing a single transcoding step.
///
/// See the module level documentation for a more comprehensive explanation of
/// this type's usage.
struct Transcoder<P, E> {
  parent: Cell<Option<P>>,
  error: Cell<Option<E>>,
  source: Cell<ErrorSource>,
}

#[derive(Clone, Copy)]
enum ErrorSource {
  Ser,
  De,
}

impl<P, E> Transcoder<P, E> {
  fn new(v: P) -> Transcoder<P, E> {
    Transcoder {
      parent: Cell::new(Some(v)),
      error: Cell::new(None),
      source: Cell::new(ErrorSource::De),
    }
  }

  fn take_parent(&self) -> P {
    match self.parent.replace(None) {
      Some(parent) => parent,
      None => panic!("parent already taken from transcoder"),
    }
  }

  fn capture_source(&self, source: ErrorSource) {
    self.source.set(source)
  }

  fn error_source(&self) -> ErrorSource {
    self.source.get()
  }

  fn capture_error(&self, error: Option<E>) {
    self.error.set(error)
  }

  fn into_error(self) -> Option<E> {
    self.error.into_inner()
  }
}

/// Handles the capture and mapping of serializer errors in visitor methods that
/// only need to forward simple values.
fn forward_value<S, E, F>(t: &Transcoder<S, S::Error>, f: F) -> Result<S::Ok, E>
where
  S: Serializer,
  E: de::Error,
  F: FnOnce(S) -> Result<S::Ok, S::Error>,
{
  let serializer = t.take_parent();
  match f(serializer) {
    Ok(value) => Ok(value),
    Err(serr) => {
      t.capture_source(ErrorSource::Ser);
      t.capture_error(Some(serr));
      Err(de::Error::custom(TRANSLATION_FAILED))
    }
  }
}

impl<'de, S> de::Visitor<'de> for &Transcoder<S, S::Error>
where
  S: Serializer,
{
  type Value = S::Ok;

  fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.write_str("any supported value")
  }

  fn visit_unit<E: de::Error>(self) -> Result<Self::Value, E> {
    forward_value(self, |s| s.serialize_unit())
  }

  fn visit_bool<E: de::Error>(self, v: bool) -> Result<Self::Value, E> {
    forward_value(self, |s| s.serialize_bool(v))
  }

  fn visit_i8<E: de::Error>(self, v: i8) -> Result<Self::Value, E> {
    forward_value(self, |s| s.serialize_i8(v))
  }

  fn visit_i16<E: de::Error>(self, v: i16) -> Result<Self::Value, E> {
    forward_value(self, |s| s.serialize_i16(v))
  }

  fn visit_i32<E: de::Error>(self, v: i32) -> Result<Self::Value, E> {
    forward_value(self, |s| s.serialize_i32(v))
  }

  fn visit_i64<E: de::Error>(self, v: i64) -> Result<Self::Value, E> {
    forward_value(self, |s| s.serialize_i64(v))
  }

  fn visit_i128<E: de::Error>(self, v: i128) -> Result<Self::Value, E> {
    forward_value(self, |s| s.serialize_i128(v))
  }

  fn visit_u8<E: de::Error>(self, v: u8) -> Result<Self::Value, E> {
    forward_value(self, |s| s.serialize_u8(v))
  }

  fn visit_u16<E: de::Error>(self, v: u16) -> Result<Self::Value, E> {
    forward_value(self, |s| s.serialize_u16(v))
  }

  fn visit_u32<E: de::Error>(self, v: u32) -> Result<Self::Value, E> {
    forward_value(self, |s| s.serialize_u32(v))
  }

  fn visit_u64<E: de::Error>(self, v: u64) -> Result<Self::Value, E> {
    forward_value(self, |s| s.serialize_u64(v))
  }

  fn visit_u128<E: de::Error>(self, v: u128) -> Result<Self::Value, E> {
    forward_value(self, |s| s.serialize_u128(v))
  }

  fn visit_f32<E: de::Error>(self, v: f32) -> Result<Self::Value, E> {
    forward_value(self, |s| s.serialize_f32(v))
  }

  fn visit_f64<E: de::Error>(self, v: f64) -> Result<Self::Value, E> {
    forward_value(self, |s| s.serialize_f64(v))
  }

  fn visit_char<E: de::Error>(self, v: char) -> Result<Self::Value, E> {
    forward_value(self, |s| s.serialize_char(v))
  }

  fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
    forward_value(self, |s| s.serialize_str(v))
  }

  fn visit_bytes<E: de::Error>(self, v: &[u8]) -> Result<Self::Value, E> {
    forward_value(self, |s| s.serialize_bytes(v))
  }

  fn visit_seq<A>(self, mut input: A) -> Result<Self::Value, A::Error>
  where
    A: de::SeqAccess<'de>,
  {
    let mut output = self
      .take_parent()
      .serialize_seq(input.size_hint())
      .map_err(|serr| {
        self.capture_source(ErrorSource::Ser);
        self.capture_error(Some(serr));
        de::Error::custom(TRANSLATION_FAILED)
      })?;

    loop {
      let seed = Transcoder::new(&mut output);
      match input.next_element_seed(&seed) {
        Ok(None) => break,
        Ok(Some(())) => {}
        Err(derr) => {
          self.capture_source(seed.error_source());
          self.capture_error(seed.into_error());
          return Err(derr);
        }
      }
    }

    output.end().map_err(|serr| {
      self.capture_source(ErrorSource::Ser);
      self.capture_error(Some(serr));
      de::Error::custom(TRANSLATION_FAILED)
    })
  }

  fn visit_map<A>(self, mut input: A) -> Result<Self::Value, A::Error>
  where
    A: de::MapAccess<'de>,
  {
    let mut output = self
      .take_parent()
      .serialize_map(input.size_hint())
      .map_err(|serr| {
        self.capture_source(ErrorSource::Ser);
        self.capture_error(Some(serr));
        de::Error::custom(TRANSLATION_FAILED)
      })?;

    loop {
      let key_seed = KeySeed(Transcoder::new(&mut output));
      match input.next_key_seed(&key_seed) {
        Ok(None) => break,
        Ok(Some(())) => {}
        Err(derr) => {
          self.capture_source(key_seed.0.error_source());
          self.capture_error(key_seed.0.into_error());
          return Err(derr);
        }
      }

      let value_seed = ValueSeed(Transcoder::new(&mut output));
      if let Err(derr) = input.next_value_seed(&value_seed) {
        self.capture_source(value_seed.0.error_source());
        self.capture_error(value_seed.0.into_error());
        return Err(derr);
      }
    }

    output.end().map_err(|serr| {
      self.capture_source(ErrorSource::Ser);
      self.capture_error(Some(serr));
      de::Error::custom(TRANSLATION_FAILED)
    })
  }
}

/// Handles the capture and mapping of errors when forwarding the next value
/// from a deserializer to a sequence or map serializer method. As these methods
/// require `Serialize` values, we automatically wrap the deserializer with an
/// inner transcoder and propagate its captured error.
fn forward_next<'de, D, F, S, E>(t: &Transcoder<S, E>, d: D, f: F) -> Result<(), D::Error>
where
  D: Deserializer<'de>,
  F: FnOnce(S, &Transcoder<D, D::Error>) -> Result<(), E>,
{
  let serializer = t.take_parent();
  let forwarder = Transcoder::new(d);
  match f(serializer, &forwarder) {
    Ok(()) => Ok(()),
    Err(serr) => {
      t.capture_source(forwarder.error_source());
      t.capture_error(Some(serr));
      match forwarder.into_error() {
        Some(derr) => Err(derr),
        None => Err(de::Error::custom(TRANSLATION_FAILED)),
      }
    }
  }
}

impl<'de, S> DeserializeSeed<'de> for &Transcoder<&mut S, S::Error>
where
  S: SerializeSeq,
{
  type Value = ();

  fn deserialize<D>(self, d: D) -> Result<Self::Value, D::Error>
  where
    D: Deserializer<'de>,
  {
    forward_next(self, d, |s, f| s.serialize_element(f))
  }
}

struct KeySeed<'a, S: SerializeMap>(Transcoder<&'a mut S, S::Error>);

impl<'de, S> DeserializeSeed<'de> for &KeySeed<'_, S>
where
  S: SerializeMap,
{
  type Value = ();

  fn deserialize<D>(self, d: D) -> Result<Self::Value, D::Error>
  where
    D: Deserializer<'de>,
  {
    forward_next(&self.0, d, |s, f| s.serialize_key(f))
  }
}

struct ValueSeed<'a, S: SerializeMap>(Transcoder<&'a mut S, S::Error>);

impl<'de, S> DeserializeSeed<'de> for &ValueSeed<'_, S>
where
  S: SerializeMap,
{
  type Value = ();

  fn deserialize<D>(self, d: D) -> Result<Self::Value, D::Error>
  where
    D: Deserializer<'de>,
  {
    forward_next(&self.0, d, |s, f| s.serialize_value(f))
  }
}

impl<'de, D> Serialize for Transcoder<D, D::Error>
where
  D: Deserializer<'de>,
{
  fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    let deserializer = self.take_parent();
    let visitor = Transcoder::new(s);
    match deserializer.deserialize_any(&visitor) {
      Ok(value) => Ok(value),
      Err(derr) => {
        self.capture_source(visitor.error_source());
        self.capture_error(Some(derr));
        match visitor.into_error() {
          Some(serr) => Err(serr),
          None => Err(ser::Error::custom(TRANSLATION_FAILED)),
        }
      }
    }
  }
}

/// A deserialized value referencing borrowed data.
///
/// `Value` supports use cases where a deserializer cannot provide direct
/// transcoding and must deserialize into an in-memory value. For example, a
/// deserializer may handle sequences of inputs by providing an `Iterator` of
/// values, rather than allowing the deserializer to be reused or providing an
/// `Iterator` of deserializers.
///
/// `Value` is optimized for transcoding use cases rather than generic use. For
/// example:
///
/// - It leverages Serde's zero-copy deserialization capabilities, which limits
///   the lifetime of the value as well as the types of inputs it can
///   deserialize from.
///
/// - It represents maps as `Vec`s of key-value pairs, and therefore does not
///   facilitate random access to map values.
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
    use Value::*;
    match self {
      Unit => s.serialize_unit(),
      Bool(b) => s.serialize_bool(*b),
      I8(n) => s.serialize_i8(*n),
      I16(n) => s.serialize_i16(*n),
      I32(n) => s.serialize_i32(*n),
      I64(n) => s.serialize_i64(*n),
      I128(n) => s.serialize_i128(*n),
      U8(n) => s.serialize_u8(*n),
      U16(n) => s.serialize_u16(*n),
      U32(n) => s.serialize_u32(*n),
      U64(n) => s.serialize_u64(*n),
      U128(n) => s.serialize_u128(*n),
      F32(f) => s.serialize_f32(*f),
      F64(f) => s.serialize_f64(*f),
      Char(c) => s.serialize_char(*c),
      String(v) => v.serialize(s),
      Bytes(v) => v.serialize(s),
      Seq(v) => v.serialize(s),
      Map(m) => {
        let mut map = s.serialize_map(Some(m.len()))?;
        for (k, v) in m {
          map.serialize_entry(k, v)?;
        }
        map.end()
      }
    }
  }
}

/// Implements methods of a [`serde::de::Visitor`] that do nothing more than
/// construct a value and shove it into an `Ok` variant.
///
/// This macro is non-hygienic, and not intended for use outside of this module.
macro_rules! xt_transcode_impl_value_visitors {
  ($($name:ident($($arg:ident: $ty:ty)?) => $result:expr;)*) => {
    $(
      fn $name<E: de::Error>(self, $($arg: $ty)?) -> Result<Self::Value, E> {
        Ok($result)
      }
    )*
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

      fn visit_seq<A: de::SeqAccess<'a>>(self, mut v: A) -> Result<Self::Value, A::Error> {
        let mut vec = Vec::with_capacity(v.size_hint().unwrap_or(0));
        while let Some(e) = v.next_element()? {
          vec.push(e)
        }
        Ok(Value::Seq(vec))
      }

      fn visit_map<A: de::MapAccess<'a>>(self, mut v: A) -> Result<Self::Value, A::Error> {
        let mut vec = Vec::with_capacity(v.size_hint().unwrap_or(0));
        while let Some(entry) = v.next_entry()? {
          vec.push(entry)
        }
        Ok(Value::Map(vec))
      }
    }

    d.deserialize_any(Visitor)
  }
}
