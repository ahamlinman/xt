use criterion::{black_box, criterion_group, criterion_main, Criterion};

use xt::Format;

criterion_main!(small);

criterion_group! {
	name = small;
	config = Criterion::default();
	targets = small_json, small_yaml, small_toml, small_msgpack
}

macro_rules! xt_benchmark {
	(
		name = $name:ident;
		sources = $($source:ident),+;
		loader = $loader:path;
		translation = $from:path => $to:path;
		$(group_config { $($setting_name:ident = $setting_value:expr;)* })?
	) => {
		fn $name(c: &mut Criterion) {
			let mut group = c.benchmark_group(stringify!($name));
			let input = $loader($from);

			$($(group.$setting_name($setting_value);)*)?

			$(group.bench_function(stringify!($source), |b| {
				b.iter(|| {
					xt_benchmark!(@translate_fn $source)(
						&*input,
						black_box(Some($from)),
						black_box($to),
						std::io::sink(),
					)
				})
			});)+

			group.finish();
		}
	};
	(@translate_fn buffer) => { xt::translate_slice };
	(@translate_fn reader) => { xt::translate_reader };
}

xt_benchmark! {
	name = small_json;
	sources = buffer, reader;
	loader = load_small_data;
	translation = Format::Json => Format::Msgpack;
}

xt_benchmark! {
	name = small_yaml;
	sources = buffer, reader;
	loader = load_small_data;
	translation = Format::Yaml => Format::Json;
}

xt_benchmark! {
	name = small_toml;
	sources = buffer;
	loader = load_small_data;
	translation = Format::Toml => Format::Json;
}

xt_benchmark! {
	name = small_msgpack;
	sources = buffer, reader;
	loader = load_small_data;
	translation = Format::Msgpack => Format::Json;
}

fn load_small_data(format: Format) -> Vec<u8> {
	// The Kubernetes Job expands to a few hundred bytes regardless of format.
	load_test_data(include_bytes!("k8s-job.json"), format, 512)
}

fn load_test_data(input: &[u8], format: Format, capacity: usize) -> Vec<u8> {
	let mut output = Vec::with_capacity(capacity);
	xt::translate_slice(input, Some(Format::Json), format, &mut output)
		.expect("failed to translate test data");
	output
}
