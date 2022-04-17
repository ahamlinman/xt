use std::time::Duration;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

use xt::{Format, InputHandle};

criterion_main!(small, large);

criterion_group! {
  name = small;
  config = Criterion::default();
  targets = small_json_to_msgpack,
            small_yaml_to_json,
            small_toml_to_json,
            small_msgpack_to_json,
}

criterion_group! {
  name = large;
  config = Criterion::default().measurement_time(Duration::from_secs(30));
  targets = large_json_to_msgpack,
            large_yaml_to_json,
            large_toml_to_json,
            large_msgpack_to_json,
}

macro_rules! xt_benchmark {
  (
    name        = $name:ident;
    loader      = $loader:path;
    translation = $from:path => $to:path;
    sources     = $($source:ident),+;
    $(group_config { $($setting_name:ident = $setting_value:expr;)* })?
  ) => {
    fn $name(c: &mut Criterion) {
      let mut group = c.benchmark_group(stringify!($name));
      let input = $loader($from);

      $($(group.$setting_name($setting_value);)*)?

      $(
        group.bench_function(stringify!($source), |b| {
          b.iter(|| {
            xt::translate(
              xt_benchmark!(@input_handle $source &*input),
              black_box(Some($from)),
              black_box($to),
              std::io::sink(),
            )
          })
        });
      )+

      group.finish();
    }
  };
  (@input_handle buffer $input:expr) => { InputHandle::from_buffer($input) };
  (@input_handle reader $input:expr) => { InputHandle::from_reader($input) };
}

xt_benchmark! {
  name        = small_json_to_msgpack;
  loader      = load_small_data;
  translation = Format::Json => Format::Msgpack;
  sources     = buffer, reader;
}

xt_benchmark! {
  name        = small_yaml_to_json;
  loader      = load_small_data;
  translation = Format::Yaml => Format::Json;
  sources     = buffer;
}

xt_benchmark! {
  name        = small_toml_to_json;
  loader      = load_small_data;
  translation = Format::Toml => Format::Json;
  sources     = buffer;
}

xt_benchmark! {
  name        = small_msgpack_to_json;
  loader      = load_small_data;
  translation = Format::Msgpack => Format::Json;
  sources     = buffer, reader;
}

xt_benchmark! {
  name        = large_json_to_msgpack;
  loader      = load_large_data;
  translation = Format::Json => Format::Msgpack;
  sources     = buffer, reader;
}

xt_benchmark! {
  name        = large_yaml_to_json;
  loader      = load_large_data;
  translation = Format::Yaml => Format::Json;
  sources     = buffer;
  group_config {
    sample_size = 50;
  }
}

xt_benchmark! {
  name        = large_toml_to_json;
  loader      = load_large_data;
  translation = Format::Toml => Format::Json;
  sources     = buffer;
  group_config {
    measurement_time = Duration::from_secs(60);
    sample_size      = 20;
  }
}

xt_benchmark! {
  name        = large_msgpack_to_json;
  loader      = load_large_data;
  translation = Format::Msgpack => Format::Json;
  sources     = buffer, reader;
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
