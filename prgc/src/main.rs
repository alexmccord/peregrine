use std::path::PathBuf;

use clap::Parser;

#[derive(clap::Parser)]
pub struct Cli {
    pub input: PathBuf,
    #[arg(long = "")]
    pub output: PathBuf,
}

fn main() {
    let opts = Cli::parse();
}
