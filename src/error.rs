//! Barebones helpers for error handling.

use std::error::Error as StdError;
use std::fmt::Display;
use std::result;

/// Alias for a [`Result`](result::Result) with the error type [`xt::Error`](Error).
pub type Result<T> = result::Result<T, Error>;

/// An error encountered during translation.
#[derive(Debug)]
pub struct Error(Box<dyn StdError + Send + Sync + 'static>);

impl AsRef<dyn StdError + Send + Sync> for Error {
	fn as_ref(&self) -> &(dyn StdError + Send + Sync + 'static) {
		self.0.as_ref()
	}
}

impl Display for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		Display::fmt(&self.0, f)
	}
}

/// Box any error into an [`Error`].
///
/// **This impl is not stable.** External code must not rely on the ability to
/// construct an [`Error`] from any particular value.
#[doc(hidden)]
impl<T> From<T> for Error
where
	T: Into<Box<dyn StdError + Send + Sync + 'static>>,
{
	fn from(err: T) -> Self {
		Self(err.into())
	}
}
