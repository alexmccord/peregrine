use std::{path::PathBuf, process};

use clap;

use peregrine::pipeline::{
    driver::{BuildDriver, BuildTask},
    resolver::FileSystemIO,
};

#[derive(clap::Parser, Debug)]
struct Build {
    #[arg(short = 'C')]
    pub dir: Option<PathBuf>,
}

#[derive(clap::Parser, Debug)]
enum Command {
    Build(Build),
}

fn main() {
    let args = clap::Parser::parse();

    match args {
        Command::Build(b) => build(b),
    }
}

fn build(build: Build) {
    let prg = build.dir.or(std::env::current_dir().ok()).unwrap();

    let mut driver = BuildDriver::new(FileSystemIO);
    driver.submit_task(BuildTask::BuildProject(prg));

    if let Err(e) = driver.execute() {
        eprintln!("{e}");
        process::exit(1);
    }
}
