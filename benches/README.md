# xt Benchmarks

xt includes a [Criterion.rs][criterion] benchmark suite to make it easier to
analyze the potential performance impacts of changes.

[criterion]: https://github.com/bheisler/criterion.rs

Each benchmark loads a test input into memory, and measures the time required to
translate it from each supported input format to a specific output format
(MessagePack for JSON inputs, JSON for everything else). For input formats that
support streaming, the benchmark compares the performance of the streaming and
non-streaming implementations.

Keep in mind that the goal is **not** to compare the performance of particular
input and output format combinations, but to understand how changes to xt itself
(transcoder changes, dependency updates, etc.) affect the performance of
well-known and somewhat representative use cases. Criterion.rs facilitates this
by saving the results of subsequent runs and using statistical analysis to
report significant performance changes.

## Reference

In short: run one or more benchmarks, change the code (e.g. switch branches),
and run the same benchmark(s) again. Look for the output of the second run to
tell you whether performance has significantly **improved** or **regressed**.

For best results, run benchmarks in as quiet of an environment as you can
manage, e.g. by closing all other applications before starting the run.
Criterion.rs will report the number and severity of outliers in its
measurements.

```sh
# Run all the benchmarks (1 - 2 minutes on Apple M1 Max)
cargo bench

# Run only the JSON input benchmarks (under 1 minute on Apple M1 Max)
cargo bench json
```

The argument to `cargo bench` is a substring match against the full benchmark
names of the form `{size}_{format}/{source}`.

- **size**: `small` (see below)
- **format**: A full format name as given to xt's `-f` or `-t` (e.g. `json`)
- **source**: `buffer` (non-streaming) or `reader` (streaming)

Criterion.rs will write an HTML report to `target/criterion/report/index.html`
after each run. The report provides details of the measurements from each
benchmark run, including charts and comparisons with any previous run.

## Test Inputs

The small input, `k8s-job.json`, is a simple Kubernetes `Job` that runs the
Docker `hello-world` image. Translation time is usually a few microseconds for
even the slowest input formats, so each benchmark runs in just a few seconds.
This provides relatively fast feedback as you work.

The benchmarks previously included a 20 - 30 MB large input based on a sample of
GitHub events, which was included in the xt repository (and remains in its
history) as a Zstandard compressed archive of MessagePack data. Based on the
reveal of the xz-utils backdoor that was obfuscated in part as compressed test
data, **I have chosen to temporarily eliminate the large benchmarks** until they
are reimplemented to rely exclusively on human-readable inputs, ideally without
bloating the size of xt repository checkouts.

Each benchmark loads test data into an in-memory buffer by translating a
"default" version of the input with xt. This approach reduces the size of the xt
repository and ensures that disk I/O performance does not influence the
benchmark results. However, it allows changes to xt's output formatting
(whitespace, quoting, etc.) to influence the results. I expect such changes to
be rare, at least compared to other changes whose impact is worth benchmarking.
