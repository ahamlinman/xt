## Unreleased

### Added

- **Streaming support for YAML inputs.** xt now translates unbounded streams of
  YAML documents without buffering all input into memory. Previously, xt
  attempted to fully buffer streaming YAML inputs into memory during format
  detection and translation.

### Removed

- **Acceptance of rare UTF-8 encoding quirks.** As a side effect of the new
  support for streaming YAML input, xt no longer accepts UTF-8 encoded YAML
  streams that begin with a Unicode byte order mark, even though this is allowed
  by the YAML 1.2 specification. Such inputs should be exceptionally rare in
  practice. As before, xt fully supports BOMs at the beginning of UTF-16 and
  UTF-32 YAML streams.

### Changed

- **Format detection never slurps all input into memory.** Previously, the
  format detection algorithm would occasionally attempt to buffer the full
  contents of a potentially unbounded stream into memory, causing xt to consume
  as much memory as it could before crashing. To mitigate this issue, the
  detection of TOML inputs from non-file sources is now limited to streams less
  than 2 MiB in size. As always, the format detection algorithm is unstable and
  provides best-effort results only; this limit or other details of the
  algorithm may change in the future.

## v0.13.0 (2022-11-05)

### Changed

- **Rust 1.59 or higher is officially required to build xt.** This has actually
  been the requirement for some time due to the use of dependencies and
  language features that do not work on Rust 1.56, however xt's Cargo metadata
  did not properly reflect this.

## v0.12.2 (2022-09-02)

### Changed

- **File names in error messages.** xt now prints the name of the file in which
  a file read or translation error occurred. This should make it much easier to
  identify problems with specific files in multi-file inputs.
- **Output behavior on multi-file input errors.** When an error occurs in the
  second or subsequent file of a multi-file input, xt will now try to ensure
  that the translations of any previous files are fully written to standard
  output. Previously, xt may have simply written an error message and exited
  even if some files actually translated successfully.

## v0.12.1 (2022-08-31)

### Changed

- **Improved performance for JSON file inputs.** This version of xt improves the
  performance of JSON file inputs by as much as 20%. In exchange, the rare case
  of JSON documents with UTF-8 encoding errors produces slightly less detailed
  error messages, indicating the byte position of the error rather than a line
  and column.

## v0.12.0 (2022-08-25)

### Added

- **Support for multiple input files.** xt now accepts more than one input file
  on the command line, and can translate the logical concatenation of the
  provided files to a single multi-document output. This should make it much
  easier, for example, to translate large repositories of YAML or TOML
  configuration files to JSON for bulk processing with `jq` or other tools.

## v0.11.0 (2022-08-24)

### Fixed

- **Issues with YAML input and output.** This version of xt upgrades to the
  latest version of the third-party `serde_yaml` library, which plays a critical
  role in xt's YAML support. The upgrade fixes a number of potential bugs and
  vulnerabilities in the handling of YAML input and output, including cases
  where xt may have produced output incompatible with other YAML parsers by
  emitting unescaped control characters in strings.

### Changed

- **Formatting of YAML output.** The `serde_yaml` upgrade described above also
  introduced several noticeable changes to the formatting of YAML output,
  including differences in the indentation of list items and broader use of
  escape sequences for certain characters.

## v0.10.0 (2022-06-12)

### Fixed

- **Exit behavior on broken pipe errors.** Previous versions of xt treated
  broken pipes as a non-error condition and exited successfully. This version
  handles broken pipes in a manner similar to other command line tools,
  particularly on Unix and Unix-like systems, treating them properly as errors.
  This may change the behavior of scripts that modify the normal return values
  of pipelines; for example, Bash scripts with the `pipefail` option enabled
  may now report a nonzero status when a broken pipe causes xt to exit before
  fully translating its input.

## v0.9.1 (2022-05-15)

### Fixed

- **Crate size on crates.io.** The v0.9.0 release on crates.io unintentionally
  included large binary files used for benchmarking. This version should take
  things back down to a reasonable size.

## v0.9.0 (2022-04-24)

### Added

- **Format detection for streaming input.** For the subset of input formats
  that support streaming, xt can now perform automatic format detection without
  buffering the full input stream in memory first. This means that for many
  unbounded JSON and MessagePack inputs, xt no longer requires an explicit `-f`
  to prevent holding up pipelines or consuming unreasonable amounts of memory.
  Note that xt will fall back to buffering the full input stream when it cannot
  detect the stream as JSON or MessagePack, and that MessagePack detection in
  particular does not cover all possible MessagePack inputs.

### Fixed

- **Case sensitivity in file extension matching.** xt now correctly detects
  file extensions containing capital ASCII characters, rather than falling back
  to parser-based format detection.

### Changed

- The parser-based format detection algorithm has changed to account for the
  new streaming input support. Note that this algorithm is unstable and
  provides best-effort results only.

## v0.8.1 (2022-04-15)

### Fixed

- **Awkward messages for some translation errors.** Previous versions of xt
  appended a spurious "translation failed" message to certain kinds of input
  errors. This version will only include this message in the context of output
  errors, as originally intended.

## v0.8.0 (2022-04-10)

### Added

- **xt is now available on crates.io!** You can now install xt with a simple
  `cargo install xt`, without having to reference the full URL of the GitHub
  repository.

### Changed

- **The tool is no longer called jyt.** In anticipation of publishing this tool
  to crates.io, I discovered that its previous name had recently been acquired
  by another project. In addition to differentiating from this unrelated
  project, the new name better reflects xt's expansion into MessagePack
  support, and is even shorter to type. All references to the tool's name have
  been updated across the code, documentation, and test data.

## v0.7.4 (2022-04-10)

### Changed

- Several parts of the codebase, including most of the core translation logic,
  have been cleaned up in an attempt to make them easier for developers to
  understand. Testing has not identified any notable performance or functional
  impact associated with these changes.

## v0.7.3 (2022-04-09)

### Fixed

- **Inconsistent nesting depth enforcement for MessagePack input.** The tool
  now more reliably avoids hard crashes (due to stack overflow) when presented
  with highly nested MessagePack inputs. Previously, the level of protection
  against these inputs depended on whether the streaming input feature was
  used.

## v0.7.2 (2022-01-20)

### Changed

- **Condensed help with `-h`.** The tool now distinguishes between the `-h` and
  `--help` flags, and omits the full usage summary and list of known formats
  when invoked with `-h`. This is more consistent with the behavior of other
  Rust CLI tools.
- **Improved error output for invalid format names.** The error message now
  explicitly references the `--help` flag, which continues to print the full
  list of format names and supported features.

## v0.7.1 (2021-10-25)

### Changed

- **Migrated to Rust 2021.** The tool now requires Rust 1.56.0 or later to
  build.

## v0.7.0 (2021-10-10)

### Added

- **Streaming JSON and MessagePack translation.** The tool can now accept
  unbounded JSON and MessagePack input streams, and will translate each
  document in the stream as it appears. Previously, streaming was impossible as
  the tool would always attempt to buffer all input into memory before
  translating it. The current implementation of format auto detection is
  incompatible with streaming, and must be bypassed with an explicit `-f`
  option.
- **Format detection for `.msgpack` files.** While the MessagePack
  specification does not define a file extension for files containing pure
  MessagePack content, some users have informally adopted the `.msgpack`
  extension. The tool now assumes that such files contain MessagePack input and
  skips the parser-based auto detection algorithm.

### Changed

- The parser-based format auto detection algorithm has changed to account for
  new features in this release. Note that this algorithm is unstable and
  provides best-effort results only.

### Fixed

- **Performance issues for multi-document JSON input.** Previous versions of
  the tool sometimes exhibited severe performance problems when translating
  multi document JSON inputs, depending on the input source and use of format
  auto detection.
- **Noisy handling of broken pipes.** Previous versions of the tool often
  panicked with broken pipe errors when a downstream program like `less` exited
  before reading all of its input, leading to unwanted extraneous output on
  stderr. This version is expected to suppress these errors and exit
  successfully. (**Note (2022-06-12):** The behavior of exiting successfully is
  incorrect and is fixed in later versions of the tool.)
- **Support for UTF-16 and UTF-32 YAML input.** Previous versions of the tool
  required UTF-8 encoding for all YAML inputs. This version handles all text
  encodings required by the YAML 1.2 specification, with a caveat: while the
  presence of a byte order mark (BOM) at the start of a YAML stream is
  supported, repeated BOMs at the beginning of subsequent documents in the
  stream are not supported, despite the fact that YAML 1.2 allows them.

## v0.6.0 (2021-08-20)

### Added

- **MessagePack translation.** The tool now supports [MessagePack][msgpack] as
  a multi document input and output format. This makes it a great debugging aid
  for MessagePack-based file formats and protocols.

[msgpack]: https://msgpack.org/

### Changed

- The parser-based auto detection algorithm has changed to provide limited
  detection support for certain kinds of MessagePack input. Note that this
  algorithm is unstable and provides best-effort results only.

## v0.5.1 (2021-07-04)

### Fixed

- **Invalid TOML output for non-map roots.** Valid TOML files must deserialize
  to a map ("table" in TOML terms). Previous versions of the tool could produce
  nonsensical TOML files from inputs containing other types like arrays or
  strings at the top level. This version will fail to translate such inputs and
  instead emit an error.

## v0.5.0 (2021-07-04)

### Added

- **Multi document JSON and YAML translation.** When a JSON or YAML input
  contains multiple concatenated documents, the tool will now translate all of
  them.  TOML does not support the concept of multiple documents in a stream,
  so attempts to translate multi document inputs to TOML will produce an error.
- **Parser-based format auto detection.** The tool can now detect the format of
  non-file inputs through automatic parsing trials, rather than always assuming
  YAML as the format for standard input or files without recognized extensions.
  Note that the parser based auto detection algorithm is unstable and provides
  best-effort results only.

### Removed

- **Pretty printed JSON output to terminals.** Previous versions of the tool
  produced pretty or compact JSON output depending on whether the destination
  was detected as a terminal. This version always produces compact JSON output,
  with multiple documents in a stream delimited by newlines. Users who require
  pretty printed JSON should process the output with an external tool like
  `jq`.

## v0.4.9 (2021-05-31)

### Changed

- **Trailing newlines for compact JSON outputs.** This improves consistency
  between pretty and compact JSON outputs.

## v0.4.8 (2021-05-20)

### Changed

- **Improved performance for file inputs.** Specifically, the tool now
  pre-populates memory maps, creating a noticeable difference for small files.

## v0.4.7 (2021-05-16)

### Changed

- **Link time optimization for release builds.** This improves performance and
  reduces the size of the binary, with little impact to build times.

## v0.4.6 (2021-03-28)

### Fixed

- **Misplaced newlines in pretty JSON output.** Previous versions of the tool
  incorrectly emitted the trailing newline at a random place in the output.

## v0.4.5 (2021-03-13)

### Changed

- **Reduced binary size.** Specifically, the tool now aborts with a core dump
  on Rust panics rather than unwinding the stack.

## v0.4.4 (2021-03-13)

### Fixed

- **Failing exit status for `--help` and `--version`.** This resolves an error
  introduced in the previous release.

## v0.4.3 (2021-03-13)

### Changed

- **More consistent error messages.** The error message for invalid arguments
  is now more consistent with other error messages.

## v0.4.2 (2021-03-12)

### Changed

- **Improved performance for YAML file inputs.** Specifically, the tool now
  uses memory maps for YAML files rather than buffering them manually.

## v0.4.1 (2021-03-12)

### Fixed

- **Performance issues for JSON input.** Previous versions of the tool failed
  to apply buffering to inputs and outputs, forcing the JSON parser to make
  significantly more system calls than necessary.

## v0.4.0 (2021-03-08)

### Changed

- **Nicer formatting for error messages.** Previous versions of the tool
  produced Rust panic messages for most failures. This version prints most
  error messages as a single line to standard error.

### Fixed

- **Handling of memory map failures.** Previous versions of the tool could not
  handle input from process substitutions, named pipes, or other unmappable
  "file" inputs. This version will fall back to normal buffering when
  necessary.

## v0.3.2 (2021-03-07)

### Added

- **Short format names.** The tool now recognizes the first letter of each
  format name as a valid value for the `-f` and `-t` options. For example,
  `-ft` is now equivalent to `-f toml`.

## v0.3.1 (2021-03-07)

### Added

- **JSON formatting based on output destination.** The tool will now pretty
  print JSON when it detects that standard output is a terminal, but will
  continue to print compact JSON output to files or pipes.

## v0.3.0 (2021-03-07)

This is the initial release of the second generation of my data format
translation tool, implementing translation between JSON, YAML, and TOML.
Improvements over the first generation include:

- More efficient translation logic that directly streams values between parsers
  and emitters. In many cases, the tool can now translate input without
  buffering it into an intermediate value.
- A simplified CLI interface, including JSON as the default output format.
- Memory mapping support for file inputs, improving performance compared to
  manual buffering.
- Full support for translating JSON and YAML inputs to TOML regardless of how
  input values are ordered. The TOML output will buffer and rearrange values as
  necessary to support TOML's ordering requirements.

## v0.2.x and Below

v0.1.x and v0.2.x releases represent the first generation of my data format
translation tool, which shares no common history with the ground-up rewrite
initially released as v0.3.0. No changelogs will be written or maintained for
these early versions, which are maintained for posterity in the `v0.2` branch
of this repository.
