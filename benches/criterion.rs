use std::time::Duration;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

use xt::{Format, InputHandle};

criterion_main!(small, large);

criterion_group! {
  name = small;
  config = Criterion::default();
  targets = bench_small_json_input,
            bench_small_yaml_input,
            bench_small_toml_input,
            bench_small_msgpack_input
}

criterion_group! {
  name = large;
  config = Criterion::default().measurement_time(Duration::from_secs(30));
  targets = bench_large_json_input,
            bench_large_yaml_input,
            bench_large_toml_input,
            bench_large_msgpack_input
}

fn bench_small_json_input(c: &mut Criterion) {
  let mut group = c.benchmark_group("small_json");
  let input = load_small_data(Format::Json);

  group.bench_function("buffer_to_msgpack", |b| {
    b.iter(|| {
      xt::translate(
        InputHandle::from_buffer(&*input),
        black_box(Some(Format::Json)),
        black_box(Format::Msgpack),
        std::io::sink(),
      )
    })
  });

  group.bench_function("reader_to_msgpack", |b| {
    b.iter(|| {
      xt::translate(
        InputHandle::from_reader(&*input),
        black_box(Some(Format::Json)),
        black_box(Format::Msgpack),
        std::io::sink(),
      )
    })
  });

  group.finish();
}

fn bench_small_yaml_input(c: &mut Criterion) {
  let mut group = c.benchmark_group("small_yaml");
  let input = load_small_data(Format::Yaml);

  group.bench_function("buffer_to_json", |b| {
    b.iter(|| {
      xt::translate(
        InputHandle::from_buffer(&*input),
        black_box(Some(Format::Yaml)),
        black_box(Format::Json),
        std::io::sink(),
      )
    })
  });

  group.finish();
}

fn bench_small_toml_input(c: &mut Criterion) {
  let mut group = c.benchmark_group("small_toml");
  let input = load_small_data(Format::Toml);

  group.bench_function("buffer_to_json", |b| {
    b.iter(|| {
      xt::translate(
        InputHandle::from_buffer(&*input),
        black_box(Some(Format::Toml)),
        black_box(Format::Json),
        std::io::sink(),
      )
    })
  });

  group.finish();
}

fn bench_small_msgpack_input(c: &mut Criterion) {
  let mut group = c.benchmark_group("small_msgpack");
  let input = load_small_data(Format::Msgpack);

  group.bench_function("buffer_to_json", |b| {
    b.iter(|| {
      xt::translate(
        InputHandle::from_buffer(&*input),
        black_box(Some(Format::Msgpack)),
        black_box(Format::Json),
        std::io::sink(),
      )
    })
  });

  group.bench_function("reader_to_json", |b| {
    b.iter(|| {
      xt::translate(
        InputHandle::from_reader(&*input),
        black_box(Some(Format::Msgpack)),
        black_box(Format::Json),
        std::io::sink(),
      )
    })
  });

  group.finish();
}

fn bench_large_json_input(c: &mut Criterion) {
  let mut group = c.benchmark_group("large_json");
  let input = load_large_data(Format::Json);

  group.bench_function("buffer_to_msgpack", |b| {
    b.iter(|| {
      xt::translate(
        InputHandle::from_buffer(&*input),
        black_box(Some(Format::Json)),
        black_box(Format::Msgpack),
        std::io::sink(),
      )
    })
  });

  group.bench_function("reader_to_msgpack", |b| {
    b.iter(|| {
      xt::translate(
        InputHandle::from_reader(&*input),
        black_box(Some(Format::Json)),
        black_box(Format::Msgpack),
        std::io::sink(),
      )
    })
  });

  group.finish();
}

fn bench_large_yaml_input(c: &mut Criterion) {
  let mut group = c.benchmark_group("large_yaml");
  let input = load_large_data(Format::Yaml);

  group.sample_size(50);

  group.bench_function("buffer_to_json", |b| {
    b.iter(|| {
      xt::translate(
        InputHandle::from_buffer(&*input),
        black_box(Some(Format::Yaml)),
        black_box(Format::Json),
        std::io::sink(),
      )
    })
  });

  group.finish();
}

fn bench_large_toml_input(c: &mut Criterion) {
  let mut group = c.benchmark_group("large_toml");
  let input = load_large_data(Format::Toml);

  group.measurement_time(Duration::from_secs(60));
  group.sample_size(20);

  group.bench_function("buffer_to_json", |b| {
    b.iter(|| {
      xt::translate(
        InputHandle::from_buffer(&*input),
        black_box(Some(Format::Toml)),
        black_box(Format::Json),
        std::io::sink(),
      )
    })
  });

  group.finish();
}

fn bench_large_msgpack_input(c: &mut Criterion) {
  let mut group = c.benchmark_group("large_msgpack");
  let input = load_large_data(Format::Msgpack);

  group.bench_function("buffer_to_json", |b| {
    b.iter(|| {
      xt::translate(
        InputHandle::from_buffer(&*input),
        black_box(Some(Format::Msgpack)),
        black_box(Format::Json),
        std::io::sink(),
      )
    })
  });

  group.bench_function("reader_to_json", |b| {
    b.iter(|| {
      xt::translate(
        InputHandle::from_reader(&*input),
        black_box(Some(Format::Msgpack)),
        black_box(Format::Json),
        std::io::sink(),
      )
    })
  });

  group.finish();
}

fn load_small_data(format: Format) -> Vec<u8> {
  // The K8s data expands to just a few hundred bytes regardless of format.
  load_test_data(include_bytes!("k8s-job.msgpack.zst"), format, 512)
}

fn load_large_data(format: Format) -> Vec<u8> {
  // The GitHub data expands to somewhere between 23 - 30 MB depending on the
  // output format. 32 MiB is a nice, round number that should be big enough.
  load_test_data(
    include_bytes!("github-events.msgpack.zst"),
    format,
    32 * 1024 * 1024,
  )
}

fn load_test_data(input: &[u8], format: Format, capacity: usize) -> Vec<u8> {
  let mut output = Vec::with_capacity(capacity);

  xt::translate(
    InputHandle::from_reader(zstd::Decoder::new(input).expect("failed to create zstd decoder")),
    Some(Format::Msgpack),
    format,
    &mut output,
  )
  .expect("failed to translate test data");

  output
}
