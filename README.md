# jyt

jyt is a JSON-YAML-TOML transcoding tool. It's extremely rough but even in this
form it was able to solve an immediate need I had.

```
cargo install --git https://github.com/ahamlinman/jyt
```

## Acknowledgements

Most of the functionality of this tool comes from the [`serde_any`][serde_any]
crate, which provides exceptional support for multi-format (de)serialization in
Rust. And that's not to mention the rest of the Rust ecosystem that enabled
both that crate and my tool. I submit a humble "thank you" to the Rust
community, the shoulders of which I merely stand upon here.

[serde_any]: https://crates.io/crates/serde_any
