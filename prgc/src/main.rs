use std::path::PathBuf;
use std::process;

use clap::Parser;

use peregrine;
use peregrine::{fs::FileSystemIO, CompileOptions};

#[derive(clap::Parser)]
pub struct Cli {
    pub input: PathBuf,
    #[arg(long = "output")]
    pub output: PathBuf,
}

fn main() {
    let cli = Cli::parse();

    let options = CompileOptions {
        input: cli.input,
        output: cli.output,
    };

    match peregrine::compile(options, FileSystemIO) {
        Ok(()) => (),
        Err(e) => {
            eprintln!("{e}");
            process::exit(1);
        }
    }
}
