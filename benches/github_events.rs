use std::time::Duration;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

use xt::{Format, InputHandle};

criterion_main!(benches);

criterion_group! {
  name = benches;
  config = Criterion::default().measurement_time(Duration::from_secs(30));
  targets = bench_json_input,
            bench_yaml_input,
            bench_toml_input,
            bench_msgpack_input
}

fn bench_json_input(c: &mut Criterion) {
  let mut group = c.benchmark_group("json");
  let input = load_event_data(Format::Json);

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

fn bench_yaml_input(c: &mut Criterion) {
  let mut group = c.benchmark_group("yaml");
  let input = load_event_data(Format::Yaml);

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

fn bench_toml_input(c: &mut Criterion) {
  let mut group = c.benchmark_group("toml");
  let input = load_event_data(Format::Toml);

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

fn bench_msgpack_input(c: &mut Criterion) {
  let mut group = c.benchmark_group("msgpack");
  let input = load_event_data(Format::Msgpack);

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

static GITHUB_EVENTS_MSGPACK_ZST: &[u8] = include_bytes!("github-events.msgpack.zst");

fn load_event_data(format: Format) -> Vec<u8> {
  // Depending on the format, the input expands to around 23 - 30 MB in size
  // after decompression and translation. Let's just go with a nice, round size
  // that we know will not require further allocation.
  let mut output = Vec::with_capacity(32 * 1024 * 1024);

  xt::translate(
    InputHandle::from_reader(
      zstd::Decoder::new(GITHUB_EVENTS_MSGPACK_ZST).expect("failed to create zstd decoder"),
    ),
    Some(Format::Msgpack),
    format,
    &mut output,
  )
  .expect("failed to translate test data");

  output
}
