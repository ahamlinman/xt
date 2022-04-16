use criterion::{criterion_group, criterion_main, Criterion};

use xt::{Format, InputHandle};

criterion_main!(benches);
criterion_group!(
  benches,
  bench_yaml_slice_to_json,
  bench_msgpack_slice_to_json
);

fn bench_yaml_slice_to_json(c: &mut Criterion) {
  let input = load_event_data(Format::Yaml);
  c.bench_function("yaml slice", |b| {
    b.iter(|| {
      xt::translate(
        InputHandle::from_buffer(input.as_ref()),
        Some(Format::Yaml),
        Format::Json,
        std::io::sink(),
      )
    })
  });
}

fn bench_msgpack_slice_to_json(c: &mut Criterion) {
  let input = load_event_data(Format::Msgpack);
  c.bench_function("msgpack slice", |b| {
    b.iter(|| {
      xt::translate(
        InputHandle::from_buffer(input.as_ref()),
        Some(Format::Msgpack),
        Format::Json,
        std::io::sink(),
      )
    })
  });
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
