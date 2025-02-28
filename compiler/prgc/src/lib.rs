mod frontend;
pub use frontend::*;

pub mod ast {
    pub use prgc_ast::*;
}

pub mod fs {
    pub use prgc_fs::*;
}

pub mod sem {
    pub use prgc_sem::*;
}

pub mod syn {
    pub use parser::parse;
    pub use prgc_syn::*;
}
