//! Top-level error handling for the xt binary.

/// Formats a message to standard error, then terminates the current process
/// with exit code 1.
macro_rules! xt_bail {
	($fmt:literal $(, $($args:tt)* )?) => {{
		let _ = writeln!(
			::std::io::stderr().lock(),
			"xt error: {}",
			format_args!($fmt $(, $($args)* )?),
		);
		::std::process::exit(1);
	}};
}

/// Formats a message to standard error that includes the provided file path,
/// then terminates the current process with exit code 1.
macro_rules! xt_bail_path {
    ($path:expr, $fmt:literal $(, $($args:tt)* )?) => {{
		let _ = writeln!(
			::std::io::stderr().lock(),
			"xt error in {}: {}",
			$path,
			format_args!($fmt $(, $($args)* )?),
		);
		::std::process::exit(1);
    }};
}
