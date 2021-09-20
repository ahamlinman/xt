use std::error::Error;
use std::fmt;

use serde::{de, ser};

pub fn transcode<'de, D, E, S, F>(d: D, s: S) -> Result<(), Box<dyn Error>>
where
  D: de::Deserializer<'de, Error = E>,
  E: de::Error + 'static,
  S: ser::Serializer<Error = F>,
  F: ser::Error + 'static,
{
  match d.deserialize_any(Visitor(s)) {
    Ok(None) => Ok(()),
    Ok(Some(err)) => Err(err),
    Err(err) => Err(err.into()),
  }
}

struct Visitor<S>(S);

macro_rules! __impl_simple_visitor {
  ($visit:ident, $serialize:ident) => {
    fn $visit<E: de::Error>(self) -> Result<Self::Value, E> {
      Ok(match self.0.$serialize() {
        Ok(_) => None,
        Err(err) => Some(err.into()),
      })
    }
  };
  ($t:ty, $visit:ident, $serialize:ident) => {
    fn $visit<E: de::Error>(self, v: $t) -> Result<Self::Value, E> {
      Ok(match self.0.$serialize(v) {
        Ok(_) => None,
        Err(err) => Some(err.into()),
      })
    }
  };
}

impl<'de, S, F> de::Visitor<'de> for Visitor<S>
where
  S: ser::Serializer<Error = F>,
  F: ser::Error + 'static,
{
  type Value = Option<Box<dyn Error>>;

  fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
    write!(fmt, "any value")
  }

  __impl_simple_visitor!(bool, visit_bool, serialize_bool);

  __impl_simple_visitor!(i8, visit_i8, serialize_i8);
  __impl_simple_visitor!(i16, visit_i16, serialize_i16);
  __impl_simple_visitor!(i32, visit_i32, serialize_i32);
  __impl_simple_visitor!(i64, visit_i64, serialize_i64);
  __impl_simple_visitor!(i128, visit_i128, serialize_i128);

  __impl_simple_visitor!(u8, visit_u8, serialize_u8);
  __impl_simple_visitor!(u16, visit_u16, serialize_u16);
  __impl_simple_visitor!(u32, visit_u32, serialize_u32);
  __impl_simple_visitor!(u64, visit_u64, serialize_u64);
  __impl_simple_visitor!(u128, visit_u128, serialize_u128);

  __impl_simple_visitor!(f32, visit_f32, serialize_f32);
  __impl_simple_visitor!(f64, visit_f64, serialize_f64);

  __impl_simple_visitor!(char, visit_char, serialize_char);
  __impl_simple_visitor!(&str, visit_str, serialize_str);
  __impl_simple_visitor!(&[u8], visit_bytes, serialize_bytes);

  __impl_simple_visitor!(visit_unit, serialize_unit);
  __impl_simple_visitor!(visit_none, serialize_none);

  // Fundamental problem: We can't deal with any of the things that accept
  // another deserializer, like visit_seq and visit_map. Our setup relies on the
  // fact that the deserializer's error type has a 'static lifetime, but we are
  // not allowed to specify that here as that would make the impl stricter than
  // the trait.
  //
  // As an example, the borrow checker rejects the following:
  //
  // fn visit_some<D>(self, d: D) -> Result<Self::Value, D::Error>
  // where
  //   D: de::Deserializer<'de>,
  // {
  //   Ok(match transcode(d, self.0) {
  //     Ok(()) => None,
  //     Err(result) => Some(result),
  //   })
  // }
}
