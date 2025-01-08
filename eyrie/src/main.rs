use std::path::PathBuf;

use clap;

use peregrine::pipeline::driver::{BuildDriver, BuildTask};

#[derive(clap::Parser, Debug)]
struct Build {
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
    let prg = build.dir.unwrap();

    let mut driver = BuildDriver::new();
    driver.submit_task(BuildTask::Project(prg));
    driver.execute();
}
