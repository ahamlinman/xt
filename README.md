# jyt

jyt translates between JSON, YAML, TOML, and MessagePack.

For example, you can process a TOML file with [`jq`][jq]:

```sh
$ jyt Cargo.lock | jq -r '.package[] | select(.version | startswith("0.")).name'
atty
hashbrown
heck
# etc.
```

Or translate a configuration file from JSON to YAML for easier editing:

```sh
$ jyt -t yaml config.json > config.yaml
```

Or store a stream of JSON events as MessagePack to save space:

```sh
$ curl localhost:8001/apis/events.k8s.io/v1/events?watch | jyt -fj -tm > events.msgpack
```

## Installation

At this time, you must build jyt manually using Rust 1.56.0 or later.

```sh
cargo install --locked --git https://github.com/ahamlinman/jyt.git
```

## Usage and Features

Run `jyt --help` for full usage details.

jyt is built to "do one thing well," and tries to maintain a simple CLI
interface with limited options (for example, no control over details of the
output formatting). The most common options are `-t` to specify an output format
other than JSON, and an optional file to read from rather than standard input.

Some of jyt's notable features include:

### Efficiency

jyt builds on the powerful and unique [Serde][serde] ecosystem of streaming
serializers and deserializers for various data formats, and can often wire an
input format's parser directly to an output format's writer.

### Automatic Format Detection

When the input format is not specified with the `-f` flag, jyt will detect it
automatically via file extension, or in the worst case by slurping all input
into memory and trying different parsers until one works.

### Multi-Document Support and Streaming

When an input format allows for multiple concatenated documents in a single
input, jyt will recognize and translate every document in the input. For
example, a set of YAML documents separated by `---` markers translates to a
stream of newline-delimited JSON documents.

jyt can translate unbounded JSON and MessagePack inputs without slurping all
input into memory as long as auto-detection is disabled with an explicit `-f`
flag.

[jq]: https://stedolan.github.io/jq/
[serde]: https://serde.rs/
