use std::time::Duration;

use criterion::{criterion_group, criterion_main, Criterion};

use xt::{Format, InputHandle};

criterion_main!(benches);

criterion_group! {
  name = benches;
  config = Criterion::default()
             .warm_up_time(Duration::from_secs(10))
             .measurement_time(Duration::from_secs(60));
  targets = bench_yaml_input, bench_msgpack_input
}

fn bench_yaml_input(c: &mut Criterion) {
  let mut group = c.benchmark_group("yaml");
  let input = load_event_data(Format::Yaml);

  group.bench_function("slice_to_json", |b| {
    b.iter(|| {
      xt::translate(
        InputHandle::from_buffer(input.as_ref()),
        Some(Format::Yaml),
        Format::Json,
        std::io::sink(),
      )
    })
  });

  group.finish();
}

fn bench_msgpack_input(c: &mut Criterion) {
  let mut group = c.benchmark_group("msgpack");
  let input = load_event_data(Format::Msgpack);

  group.bench_function("slice_to_json", |b| {
    b.iter(|| {
      xt::translate(
        InputHandle::from_buffer(input.as_ref()),
        Some(Format::Msgpack),
        Format::Json,
        std::io::sink(),
      )
    })
  });

  group.bench_function("reader_to_json", |b| {
    b.iter(|| {
      xt::translate(
        InputHandle::from_reader(&*input),
        Some(Format::Msgpack),
        Format::Json,
        std::io::sink(),
      )
    })
  });

  group.finish();
}

static GITHUB_EVENTS_MSGPACK_ZST: &[u8] = include_bytes!("github-events.msgpack.zst");

fn load_event_data(format: Format) -> Vec<u8> {
  // Across all formats, the expanded and translated test data ranges from
  // around 23 - 28 MB in size.
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
