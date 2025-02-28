use clap::Parser;

use eyrie;
use eyrie::Command;

use lc::fs::FileSystemIO;

fn main() {
    let args = Command::parse();

    match args {
        Command::Build(b) => eyrie::build(b, FileSystemIO),
    }
}
