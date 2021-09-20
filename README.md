# jyt

jyt translates between a number of common serialized data formats: JSON, YAML,
TOML, and MessagePack.

Among its features, jyt…

- …tries to be as efficient as possible for every input and output combination.
- …tries to auto-detect input formats, but can be overridden with a simple flag.
- …outputs JSON by default for convenient use with [jq][jq].

## Installation

jyt requires Rust 1.53.0 or later.

```sh
cargo install --locked --git https://github.com/ahamlinman/jyt.git
```

## Usage

Run `jyt -h` for full usage details.

### Examples

Process jyt's own `Cargo.lock` file with [`jq`][jq] to find all of the pre-1.0
crates it uses. Since `.lock` is a non-standard extension, jyt will run
auto-detection to determine that it is a TOML file.

```sh
$ jyt Cargo.lock | jq -r '.package[] | select(.version | test("^0\\.")).name'
atty
dtoa
hashbrown
heck
# etc.
```

Translate an application config from JSON to YAML for easier editing. jyt will
recognize the input as JSON from the file extension and skip auto-detection. We
specify the output format using the shorthand for `-t yaml`.

```sh
$ jyt -ty config.json > config.yaml
```

Read an unbounded stream of events from a Kubernetes cluster and translate it to
MessagePack for storage. Since auto-detection requires buffering the full input,
we specify `-f json` explicitly to enable continuous streaming.

```sh
$ curl http://localhost:8001/apis/events.k8s.io/v1/events?watch | jyt -fj -tm > events.msgpack
```

[jq]: https://stedolan.github.io/jq/
