//! Support for translation between Serde data formats.

mod stream;
mod value;

pub(crate) use stream::transcode;
pub(crate) use value::Value;
