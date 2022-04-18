# xt Benchmarks

xt includes a [Criterion.rs][criterion] benchmark suite to make it easier to
analyze the potential performance impacts of changes.

[criterion]: https://github.com/bheisler/criterion.rs

Each benchmark loads one of two test inputs (small or large) into memory, and
measures the time required to translate it from each supported input format to a
specific output format (MessagePack for JSON inputs, JSON for everything else).
For input formats that support streaming, the benchmark will compare the
performance of the streaming and non-streaming implementations.

Keep in mind that the goal is **not** to compare the performance of particular
input and output format combinations, but to understand how changes to xt itself
(transcoder changes, dependency updates, etc.) affect the performance of a
well-known and somewhat representative set of use cases. Criterion.rs is
especially good at this, as it saves the result of each benchmark run for
comparison with the next, and uses statistical analysis to report significant
performance changes.

## Reference

In short: run one or more benchmarks, change the code (e.g. switch branches),
and run the same benchmark(s) again. Look for the output of the second run to
tell you whether performance has significantly **improved** or **regressed**.

For best results, run benchmarks in as quiet of an environment as you can
manage, e.g. by closing all other applications before starting the run.
Criterion.rs will report the number and severity of outliers in its
measurements.

```sh
# Run all the benchmarks (5 - 7 minutes on Apple M1 Max)
cargo bench

# Run only the small input benchmarks (1 - 2 minutes on Apple M1 Max)
cargo bench small

# Run only the large streaming MessagePack input benchmark
cargo bench large_msgpack/reader
```

The argument to `cargo bench` is a substring match against the full benchmark
names of the form `{size}_{format}/{source}`.

- **size**: `small` or `large`
- **format**: A full format name as given to xt's `-f` or `-t` (e.g. `json`)
- **source**: `buffer` (non-streaming) or `reader` (streaming)

Criterion.rs will write an HTML report to `target/criterion/report/index.html`
after each run. The report provides details of the measurements from each
benchmark run, including charts and comparisons with any previous run.

## Test Inputs

The small input, `k8s-job.msgpack.zst`, is a simple Kubernetes `Job` that runs
the Docker `hello-world` image. Translation time is just a few microseconds for
even the slowest input formats, so each benchmark runs for just a few seconds.
This is good for relatively fast feedback as you work.

The large input, `github-events.msgpack.zst`, is based on [a test file
distributed by the json-iterator project][json-iterator data] under the terms of
the MIT License included below. For TOML compatibility, I've removed null values
from the data and moved the top-level array into a top-level map with an
`events` key. Translation time varies from tens of milliseconds to a couple
seconds depending on the input format, so each benchmark runs for 30 - 60
seconds in order to gather a reasonable number of samples. This is good for
limiting the impact of per-translation overhead on the results, and for
identifying any behaviors specific to larger inputs.

Each benchmark loads its test data into an in-memory buffer by translating a
[Zstandard][zstd]-compressed version of the input with xt. This approach reduces
the size of the xt repository and ensures that disk I/O performance does not
influence the benchmark results. However, it allows changes to xt's output
formatting (whitespace, quoting, etc.) to influence the results. I expect such
changes to be rare, at least compared to other changes whose impact is worth
benchmarking.

[json-iterator data]: https://github.com/json-iterator/test-data
[zstd]: https://facebook.github.io/zstd/

## Appendix: json-iterator Test Data License

This license covers the original test data on which `github-events.msgpack.zst`
is based. See the "Test Inputs" section for details.

```
MIT License

Copyright (c) 2016 json-iterator

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
