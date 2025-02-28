use std::path::PathBuf;
use std::process;

use clap::Parser;

use prgc;
use prgc::fs;

#[derive(clap::Parser)]
pub struct Cli {
    pub input: PathBuf,
    #[arg(long = "output")]
    pub output: PathBuf,
}

fn main() {
    let cli = Cli::parse();

    let options = prgc::CompileOptions {
        input: cli.input,
        output: cli.output,
    };

    match prgc::compile(options, fs::FileSystemIO) {
        Ok(()) => (),
        Err(e) => {
            eprintln!("{e}");
            process::exit(1);
        }
    }
}
