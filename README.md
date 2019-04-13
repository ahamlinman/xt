# recompose

Recompose is a simple utility to transcode between JSON, YAML, and TOML (and
potentially other formats in the future).

As of this writing, I've built out enough of recompose to solve some immediate
needs for myself. It's rough code, but it gets the job done.

Most of the functionality of this tool comes from the [`serde_any`][serde_any]
crate, which provides exceptional support for multi-format (de)serialization in
Rust. And that's not to mention the rest of the Rust ecosystem that enabled
both that crate and my tool. I submit a humble "thank you" to the Rust
community, the shoulders of which I merely stand upon here.

```
cargo install --git https://github.com/ahamlinman/recompose
```

[serde_any]: https://crates.io/crates/serde_any
