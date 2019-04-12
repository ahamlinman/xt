use clap::{crate_version, App, Arg};

fn main() {
    App::new("recompose")
        .version(crate_version!())
        .about("Convert between serialized data formats")
        .arg(
            Arg::with_name("from")
                .long("from")
                .help("Format to convert from"),
        )
        .arg(
            Arg::with_name("to")
                .long("to")
                .required(true)
                .help("Format to convert to"),
        )
        .get_matches();
}
