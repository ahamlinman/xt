//! Top-level error handling for the xt binary.

/// Prints a message to standard error, then terminates the current process with
/// exit code 1.
///
/// `xt_bail` accepts one of two argument forms:
///
/// - A single non-literal expression that supports `Display`.
/// - A format string literal followed by a variable number of arguments, as in
///   other formatting macros.
///
/// Optionally, the argument list in either form may begin with a single
/// identifier, referencing a variable whose value displays as a file path.
macro_rules! xt_bail {
	($path:ident, $fmt:literal $(, $($args:tt)* )?) => {{
		let _ = writeln!(
			::std::io::stderr().lock(),
			"xt error in {}: {}",
			$path,
			format_args!($fmt $(, $($args)* )?),
		);
		::std::process::exit(1);
	}};
	($fmt:literal $(, $($args:tt)* )?) => {{
		let _ = writeln!(
			::std::io::stderr().lock(),
			"xt error: {}",
			format_args!($fmt $(, $($args)* )?),
		);
		::std::process::exit(1);
	}};
	($path:ident, $x:expr) => { xt_bail!($path, "{}", $x) };
	($x:expr) => { xt_bail!("{}", $x) };
}
