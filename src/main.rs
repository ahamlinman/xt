use std::error::Error;
use std::fmt;
use std::io::prelude::*;
use std::io::{stdin, stdout};

use clap::{crate_version, App, Arg};
use serde_any;
use serde_value;

fn main() {
    let matches = App::new("recompose")
        .version(crate_version!())
        .about("Convert between serialized data formats")
        .arg(
            Arg::with_name("from")
                .long("from")
                .takes_value(true)
                .required(true)
                .help("Format to convert from"),
        )
        .arg(
            Arg::with_name("to")
                .long("to")
                .takes_value(true)
                .required(true)
                .help("Format to convert to"),
        )
        .get_matches();

    let value_tree = match read_value_tree(stdin(), matches.value_of("from").unwrap()) {
        Ok(v) => v,
        Err(e) => panic!("unable to read input: {}", e),
    };

    if let Err(e) = write_value_tree(stdout(), value_tree, matches.value_of("to").unwrap()) {
        panic!("unable to write output: {}", e);
    }
}

fn read_value_tree<R: Read>(rdr: R, format: &str) -> Result<serde_value::Value, Box<dyn Error>> {
    let format = match serde_any_format(format) {
        Some(f) => f,
        None => return Err(Box::new(UnknownFormatError(String::from(format)))),
    };

    match serde_any::from_reader(rdr, format) {
        Ok(v) => Ok(v),
        Err(e) => Err(Box::new(SerdeAnyError(e))),
    }
}

fn write_value_tree<W: Write>(
    writer: W,
    value: serde_value::Value,
    format: &str,
) -> Result<(), Box<dyn Error>> {
    let format = match serde_any_format(format) {
        Some(f) => f,
        None => return Err(Box::new(UnknownFormatError(String::from(format)))),
    };

    match serde_any::to_writer_pretty(writer, &value, format) {
        Ok(_) => Ok(()),
        Err(e) => Err(Box::new(SerdeAnyError(e))),
    }
}

#[derive(Debug)]
struct SerdeAnyError(serde_any::Error);

impl fmt::Display for SerdeAnyError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Error for SerdeAnyError {
    // TODO: serde_any::Error doesn't implement std::error::Error. So returning a cause is not
    // trivial as far as I know, but would be extremely useful.
}

fn serde_any_format(format: &str) -> Option<serde_any::Format> {
    use serde_any::Format::*;

    match format {
        "json" => Some(Json),
        "yaml" => Some(Yaml),
        "toml" => Some(Toml),

        _ => None,
    }
}

#[derive(Debug)]
struct UnknownFormatError(String);

impl fmt::Display for UnknownFormatError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "unknown format {}", &self.0)
    }
}

impl Error for UnknownFormatError {}
