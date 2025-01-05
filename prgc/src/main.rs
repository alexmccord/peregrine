use std::path::PathBuf;
use std::{fs::File, io::Read};

use clap;
use peregrine::semantics::dependencies::Dependencies;
use peregrine::syntax::ast::AstAllocator;
use peregrine::syntax::parser::Parser;

#[derive(clap::Parser, Debug)]
struct Build {
    pub dir: Option<PathBuf>,
}

#[derive(clap::Parser, Debug)]
enum Command {
    Build(Build),
    Hack,
}

fn main() {
    let args = clap::Parser::parse();

    match args {
        Command::Build(b) => build(b),
        Command::Hack => hack(),
    }
}

fn build(build: Build) {
    let prg = build.dir.unwrap();

    let mut content = String::new();
    let mut file = File::open(prg).unwrap();
    file.read_to_string(&mut content)
        .expect("you idiot you used the wrong cwd or moved it somewhere else");

    let mut arena = AstAllocator::default();
    let mut parser = Parser::new(&content, &mut arena);
    let result = parser.parse();

    let deps = Dependencies::get(&result);
    for import in deps.imports {
        println!("{}", import.path.join("."));
    }
}

fn hack() {
    let cwd = std::env::current_dir().unwrap();
    let prg = cwd.join("future/Core/Prelude.prg");

    let mut content = String::new();
    let mut file = File::open(prg).unwrap();
    file.read_to_string(&mut content)
        .expect("you idiot you used the wrong cwd or moved it somewhere else");

    let mut arena = AstAllocator::default();
    let mut parser = Parser::new(&content, &mut arena);
    let result = parser.parse();
}
