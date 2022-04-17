use criterion::{black_box, criterion_group, criterion_main, Criterion};

use xt::{Format, InputHandle};

criterion_main!(k8s_job);

criterion_group! {
  name = k8s_job;
  config = Criterion::default();
  targets = bench_json_input,
            bench_yaml_input,
            bench_toml_input,
            bench_msgpack_input
}

fn bench_json_input(c: &mut Criterion) {
  let mut group = c.benchmark_group("json");
  let input = load_input_data(Format::Json);

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
  let input = load_input_data(Format::Yaml);

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
  let input = load_input_data(Format::Toml);

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
  let input = load_input_data(Format::Msgpack);

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

static K8S_JOB_MSGPACK: &[u8] = include_bytes!("k8s-job.msgpack");

fn load_input_data(format: Format) -> Vec<u8> {
  let mut output = Vec::with_capacity(4096);

  xt::translate(
    InputHandle::from_buffer(K8S_JOB_MSGPACK),
    Some(Format::Msgpack),
    format,
    &mut output,
  )
  .expect("failed to translate test data");

  output
}
