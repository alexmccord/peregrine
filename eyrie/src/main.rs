use clap::Parser;

use eyrie::{self, Command};

use peregrine::fs::FileSystemIO;

fn main() {
    let args = Command::parse();

    match args {
        Command::Build(b) => eyrie::build(b, FileSystemIO),
    }
}
