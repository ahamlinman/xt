#![no_main]

use std::io;

use libfuzzer_sys::fuzz_target;

use xt::Format;

fuzz_target!(|data: &[u8]| {
	let _ = xt::translate_slice(data, None, Format::Json, io::sink());
	let _ = xt::translate_slice(data, None, Format::Msgpack, io::sink());
	let _ = xt::translate_slice(data, None, Format::Toml, io::sink());
	let _ = xt::translate_slice(data, None, Format::Yaml, io::sink());
});
