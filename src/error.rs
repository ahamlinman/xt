use std::error;
use std::result;

/// The result produced by translation.
///
/// There is no useful `Ok` value, as the translator streams its output to a
/// writer.
pub type Result<T> = result::Result<T, Error>;

/// An error produced during translation.
pub type Error = Box<dyn error::Error>;
