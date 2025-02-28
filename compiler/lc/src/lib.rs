mod frontend;
pub use frontend::*;

pub mod ast {
    pub use lc_ast::*;
}

pub mod fs {
    pub use lc_fs::*;
}

pub mod sem {
    pub use lc_sem::*;
}

pub mod syn {
    pub use lc_syn::*;
    pub use parser::parse;
}
