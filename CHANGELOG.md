## v0.7.0 (2021-10-10)

### Added

- **Streaming JSON and MessagePack translation.** jyt can now accept unbounded
  JSON and MessagePack input streams, and will translate each document in the
  stream as it appears. Previously, streaming was impossible as jyt would always
  attempt to buffer all input into memory before translating it. The current
  implementation of format auto detection is incompatible with streaming, and
  must be bypassed with an explicit `-f` option.
- **Format detection for `.msgpack` files.** While the MessagePack specification
  does not define a file extension for files containing pure MessagePack
  content, some users have informally adopted the `.msgpack` extension. jyt now
  assumes that such files contain MessagePack input and skips the parser-based
  auto detection algorithm.

### Changed

- The parser-based format auto detection algorithm has changed to account for
  new features in this release. Note that this algorithm is unstable and
  provides best-effort results only.

### Fixed

- **Performance issues for multi-document JSON input.** Previous versions of jyt
  occasionally exhibited severe performance problems when translating multi
  document JSON inputs, depending on the input source and use of format auto
  detection.
- **Inconsistent handling of broken pipe errors.** Previous versions of jyt
  often failed with broken pipe errors when a downstream program like `less`
  exited before reading all of its input. As of this release, jyt is expected to
  suppress these errors and exit successfully.
- **Support for UTF-16 and UTF-32 YAML input.** Previous versions of jyt
  required UTF-8 encoding for all YAML inputs. As of this release, jyt handles
  all text encodings required by the YAML 1.2 specification, with some caveats.
  In particular, while jyt supports the presence of a byte order mark (BOM) at
  the start of a YAML stream, it does not currently support repeated BOMs at the
  beginning of subsequent documents in the stream, as YAML allows for.

## v0.6.0 (2021-08-20)

### Added

- **MessagePack translation.** jyt now supports [MessagePack][msgpack] as a
  multi document input and output format. This makes jyt a great debugging tool
  for MessagePack-based file formats and protocols.

[msgpack]: https://msgpack.org/

### Changed

- The parser-based auto detection algorithm has changed to provide limited
  detection support for certain kinds of MessagePack input. Note that this
  algorithm is unstable and provides best-effort results only.

## v0.5.1 (2021-07-04)

### Fixed

- **Invalid TOML output for non-map roots.** Valid TOML files must deserialize
  to a map ("table" in TOML terms). Previous versions of jyt could produce
  nonsensical TOML files from inputs containing other types like arrays or
  strings at the top level. As of this release, jyt will fail to translate these
  inputs and instead emit an error.

## v0.5.0 (2021-07-04)

### Added

- **Multi document JSON and YAML translation.** When a JSON or YAML input
  contains multiple concatenated documents, jyt will now translate all of them.
  TOML does not support the concept of multiple documents in a stream, and jyt
  will fail to translate multi document inputs to TOML.
- **Parser-based format auto detection.** jyt can now detect the format of
  non-file inputs through automatic parsing trials, rather than always assuming
  YAML as the format for standard input or files without recognized extensions.
  Note that the parser based auto detection algorithm is unstable and provides
  best-effort results only.

### Removed

- **Pretty printed JSON output to terminals.** Previous versions of jyt produced
  pretty or compact JSON output depending on whether the destination was
  detected as a terminal. As of this version, jyt always produces compact JSON
  output, with multiple documents in a stream delimited by newlines. Users who
  require pretty printed JSON should process jyt's output with an external tool
  like `jq`.

## v0.4.9 (2021-05-31)

### Changed

- **Trailing newlines for compact JSON outputs.** This improves consistency
  between pretty and compact JSON outputs.

## v0.4.8 (2021-05-20)

### Changed

- **Improved performance for file inputs.** Specifically, jyt now pre-populates
  memory maps, creating a noticeable difference for small files.

## v0.4.7 (2021-05-16)

### Changed

- **Link time optimization for release builds.** This improves performance and
  reduces the size of the jyt binary, with little impact to build times.

## v0.4.6 (2021-03-28)

### Fixed

- **Misplaced newlines in pretty JSON output.** Previous versions of jyt
  incorrectly emitted the trailing newline at a random place in the output.

## v0.4.5 (2021-03-13)

### Changed

- **Reduced binary size.** Specifically, jyt now aborts with a core dump on Rust
  panics rather than unwinding the stack.

## v0.4.4 (2021-03-13)

### Fixed

- **Failing exit status for `--help` and `--version`.** This resolves an error
  introduced in the previous release.

## v0.4.3 (2021-03-13)

### Changed

- **More consistent error messages.** The error message for invalid arguments is
  now more consistent with other error messages produced by jyt.

## v0.4.2 (2021-03-12)

### Changed

- **Improved performance for YAML file inputs.** Specifically, jyt now uses
  memory maps for YAML files rather than buffering them manually.

## v0.4.1 (2021-03-12)

### Fixed

- **Performance issues for JSON input.** Previous versions of jyt failed to
  apply buffering to inputs and outputs, forcing the JSON parser to make
  significantly more system calls than necessary.

## v0.4.0 (2021-03-08)

### Changed

- **Nicer formatting for error messages.** Previous versions of jyt produced
  Rust panic messages for most failures. As of this release, jyt prints most
  error messages to standard error with a "jyt error" prefix.

### Fixed

- **Handling of memory map failures.** Previous versions of jyt could not handle
  input from process substitutions, named pipes, or other unmappable "file"
  inputs. As of this release, jyt will fall back to normal buffering when
  necessary.

## v0.3.2 (2021-03-07)

### Added

- **Short format names.** jyt now recognizes the first letter of each format
  name as a valid value for the `-f` and `-t` options. For example, `-ft` is now
  equivalent to `-f toml`.

## v0.3.1 (2021-03-07)

### Added

- **JSON formatting based on output destination.** jyt will now pretty print
  JSON when it detects that standard output is a terminal, but will continue to
  print compact JSON output to files or pipes.

## v0.3.0 (2021-03-07)

jyt v0.3.0 is the initial release of the second generation of the jyt tool,
implementing translation between the JSON, YAML, and TOML serialized data
formats. Improvements over the first generation of jyt include:

- More efficient translation logic that directly streams values between parsers
  and emitters. In many cases, jyt can now translate input without buffering it
  into an intermediate value.
- A simplified CLI interface, including JSON as the default output format.
- Memory mapping support for file inputs, improving performance compared to
  manual buffering.
- Full support for translating JSON and YAML inputs to TOML regardless of how
  input values are ordered. The TOML output will buffer and rearrange values as
  necessary to support TOML's ordering requirements.

## v0.2.x and Below

v0.1.x and v0.2.x releases represent the first generation of the jyt tool
(originally called "recompose"), which shares no common history with the
ground-up rewrite initially released as v0.3.0. No changelogs will be written or
maintained for these early versions, which are maintained for posterity in the
`v0.2` branch of the jyt repository.
