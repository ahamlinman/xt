# jyt

jyt translates between three common serialized data formats: JSON, YAML, and
TOML. jyt...

- ...tries to be as efficient as possible for every input and output
  combination, avoiding internal buffers except where necessary for correct
  output.
- ...autodetects input formats from file extensions, or supports a simple
  flag to set the input type.
- ...outputs JSON by default for convenient use with [jq][jq].

_Note: jyt v0.3 is a ground-up rewrite that shares no history with jyt v0.2.
The older version remains available on the `v0.2` branch._

## Install

```sh
cargo install --git https://github.com/ahamlinman/jyt.git --branch main
jyt -h
```

## Examples

Processing jyt's own `Cargo.lock` file with [`jq`][jq] to find all of the
pre-1.0 crates it uses. Since `.lock` is a non-standard extension, we use
`-f toml` to override the default YAML / JSON input parser.

```sh
$ jyt -f toml Cargo.lock | jq -r '.package[] | select(.version | test("^0\\.")).name'
dtoa
hashbrown
heck
# etc.
```

Translating an application config from JSON to YAML for easier editing. Here,
jyt will autodetect the input format, so we only need to specify the output
type.

```sh
$ jyt -t yaml config.json > config.yaml
```

[jq]: https://stedolan.github.io/jq/

## TODO

- Find a way to autodetect TOML input on stdin?
- Nicer formatted errors, instead of Rust panics.
