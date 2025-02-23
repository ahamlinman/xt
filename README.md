# xt

xt is a cross-format translator for JSON, MessagePack, TOML, and YAML.

For example, you can process a set of TOML files with [`jq`][jq]:

```sh
$ xt a/Cargo.lock b/Cargo.lock | jq -r '.package[].name' | sort -u
aho-corasick
anes
autocfg
# etc.
```

Or transform a JSON configuration file into YAML for easier editing:

```sh
$ xt -t yaml config.json > config.yaml
```

Or store an unbounded stream of JSON events as MessagePack to save space:

```sh
$ curl localhost:8001/apis/events.k8s.io/v1/events?watch | xt -tm > events.msgpack
```

[jq]: https://jqlang.org/

## Installation

xt is written in [Rust][rust] to leverage the powerful [Serde][serde] ecosystem
of data serialization and deserialization libraries.

After [installing Rust][install rust] on your system, you can install xt
[from crates.io][crate] using Cargo:

```sh
cargo install --locked xt
```

[rust]: https://www.rust-lang.org/
[serde]: https://serde.rs/
[install rust]: https://www.rust-lang.org/tools/install
[crate]: https://crates.io/crates/xt

### NetBSD

On NetBSD a pre-compiled binary is available from the official repositories. To install it, simply run:
```sh
pkgin install xt-rs
```

### Homebrew

On Linux and macOS, the pre-compiled binaries in this repo's GitHub releases can be installed with [Homebrew][homebrew]:
```sh
brew install ahamlinman/tap/xt
```

[homebrew]: https://brew.sh/

## Usage and Features

```
xt [-f format] [-t format] [file ...]
```

Or, run `xt --help` for full usage information.

xt is built to "do one thing well," and tries to maintain a minimal interface
and feature set. The most common options are `-t` to specify an output format
other than JSON, and one or more files to read from rather than standard input.

Some of xt's notable features include:

### Automatic Format Detection

When the input format is not specified with the `-f` option, xt can detect it
automatically by file extension, or by examining the content of the input stream
itself.

### Multi-Document Support

With most output formats, xt can translate multiple input files, each containing
one or more independent documents, to a single output stream. For example, a set
of YAML files with documents separated by `---` markers can translate to a
single stream of newline-delimited JSON objects. With format detection enabled,
xt can even translate input files in different formats to a single output.

### Streaming Translation

xt can translate multi-document inputs from unbounded sources like shell pipes
with minimal buffering, while still supporting features like automatic format
detection. Streaming is enabled automatically whenever it's required.

## License

xt is released under the terms of the MIT License. See `LICENSE.txt` for more
information.
