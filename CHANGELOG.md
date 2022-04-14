## Unreleased

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
- **Inconsistent handling of broken pipe errors.** Previous versions of the
  tool often failed with broken pipe errors when a downstream program like
  `less` exited before reading all of its input. This version is expected to
  suppress these errors and exit successfully.
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
