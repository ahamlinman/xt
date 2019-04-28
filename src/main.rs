use std::error::Error;
use std::fmt;
use std::io::prelude::*;
use std::io::{stdin, stdout};
use std::path::Path;

use clap::{crate_version, App, Arg};
use serde_any;
use serde_value;

const FORMATS: &[&str] = &["json", "yaml", "toml"];

fn main() {
    let matches = App::new("recompose")
        .version(crate_version!())
        .about("Convert between serialized data formats")
        .arg(
            Arg::with_name("from")
                .long("from")
                .takes_value(true)
                .possible_values(FORMATS)
                .hide_possible_values(true)
                .help("Format to convert from")
                .long_help(
                    r#"Format of the input: "json", "yaml", or "toml". If input is from a file, this flag is ignored and recompose will attempt to determine the format from the file extension. If the format is unknown, recompose will read all input into memory and try different formats in turn."#
                ),
        )
        .arg(
            Arg::with_name("to")
                .long("to")
                .takes_value(true)
                .possible_values(FORMATS)
                .hide_possible_values(true)
                .required_unless("output")
                .help("Format to convert to")
                .long_help(
                    r#"Format of the output: "json", "yaml", or "toml". If output is to a file, this flag is ignored and recompose will determine the format from the file extension."#
                ),
        )
        .arg(
            Arg::with_name("input")
                .takes_value(true)
                .help("File to read input from (default -, i.e. stdin)")
                .long_help(
                    r#"Name of a file to read input from, or "-" for stdin. Defaults to stdin if not specified."#
                ),
        )
        .arg(
            Arg::with_name("output")
                .takes_value(true)
                .help("File to write output to (default -, i.e. stdout)")
                .long_help(
                    r#"Name of a file to write output to, or "-" for stdout. Defaults to stdout if not specified."#
                ),
        )
        .get_matches();

    let value_tree = match matches.value_of("input") {
        Some(fname) if fname != "-" => read_value_tree_from_file(fname),
        _ => read_value_tree(stdin(), matches.value_of("from")),
    };

    let value_tree = match value_tree {
        Ok(v) => v,
        Err(e) => panic!("unable to read input: {}", e),
    };

    let res = match matches.value_of("output") {
        Some(fname) if fname != "-" => write_value_tree_to_file(fname, value_tree),
        _ => {
            let res = write_value_tree(stdout(), value_tree, matches.value_of("to").unwrap());
            println!();
            res
        }
    };

    if let Err(e) = res {
        panic!("unable to write output: {}", e);
    }
}

fn read_value_tree_from_file<P: AsRef<Path>>(
    path: P,
) -> Result<serde_value::Value, Box<dyn Error>> {
    match serde_any::from_file(path) {
        Ok(v) => Ok(v),
        Err(e) => Err(Box::new(SerdeAnyError(e))),
    }
}

fn read_value_tree<R: Read>(
    mut rdr: R,
    format: Option<&str>,
) -> Result<serde_value::Value, Box<dyn Error>> {
    if let Some(format) = format {
        return read_value_tree_from_known_format(rdr, format);
    }

    let mut buffer = Vec::new();
    rdr.read_to_end(&mut buffer)?;

    match serde_any::from_slice_any(buffer.as_slice()) {
        Ok(v) => Ok(v),
        Err(e) => Err(Box::new(SerdeAnyError(e))),
    }
}

fn read_value_tree_from_known_format<R: Read>(
    rdr: R,
    format: &str,
) -> Result<serde_value::Value, Box<dyn Error>> {
    let format = match serde_any_format(format) {
        Some(f) => f,
        None => return Err(Box::new(UnknownFormatError(String::from(format)))),
    };

    match serde_any::from_reader(rdr, format) {
        Ok(v) => Ok(v),
        Err(e) => Err(Box::new(SerdeAnyError(e))),
    }
}

fn write_value_tree_to_file<P: AsRef<Path>>(
    path: P,
    value: serde_value::Value,
) -> Result<(), Box<dyn Error>> {
    match serde_any::to_file_pretty(path, &value) {
        Ok(_) => Ok(()),
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
