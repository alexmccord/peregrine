use std::path::PathBuf;

use crate::{fs::FileResolver, syn};

mod error;
pub use error::*;

#[derive(Debug)]
pub struct CompileOptions {
    /// A path to the input .prg file to compile.
    pub input: PathBuf,
    /// A path to the output object file to write to.
    pub output: PathBuf,
}

pub fn compile<R: FileResolver>(options: CompileOptions, resolver: R) -> Result<()> {
    let ast = syn::parse(resolver.read(&options.input)?);

    Ok(())
}
