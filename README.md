# jyt

jyt translates between three common serialized data formats: JSON, YAML, and
TOML.

Among its features, jyt…

- …tries to be as efficient as possible for every input and output
  combination.
- …tries to autodetect input formats from file extensions, but can be
  overridden with a simple flag.
- …outputs JSON by default for convenient use with [jq][jq].

## Installation

```sh
cargo install --locked --git https://github.com/ahamlinman/jyt.git --branch main
```

## Usage

Run `jyt -h` for full usage details.

### Examples

Process jyt's own `Cargo.lock` file with [`jq`][jq] to find all of the
pre-1.0 crates it uses. Since `.lock` is a non-standard extension, we use the
shorthand for `-f toml` to override the default YAML / JSON input parser.

```sh
$ jyt -ft Cargo.lock | jq -r '.package[] | select(.version | test("^0\\.")).name'
dtoa
hashbrown
heck
# etc.
```

Translate an application config from JSON to YAML for easier editing. Here,
jyt will autodetect the input format, so we only need to specify the output
type, which we do using the shorthand for `-t yaml`.

```sh
$ jyt -ty config.json > config.yaml
```

[jq]: https://stedolan.github.io/jq/

## TODO

- Find a way to autodetect TOML input on stdin?
