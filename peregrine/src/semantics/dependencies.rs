use crate::syntax::ast;

pub struct Dependencies<'ast> {
    pub imports: Vec<&'ast ast::Import>,
}

impl<'ast> Dependencies<'ast> {
    fn new(imports: Vec<&'ast ast::Import>) -> Dependencies<'ast> {
        Dependencies { imports }
    }

    pub fn get(program: &'ast ast::Program<'ast>) -> Dependencies<'ast> {
        let mut vec = Vec::default();

        for &decl in &program.decls {
            if let ast::Decl::Import(import) = program.get_decl(decl) {
                vec.push(import)
            }
        }

        Dependencies::new(vec)
    }
}

#[cfg(test)]
mod tests {
    use super::Dependencies;
    use crate::syntax::{ast::AstAllocator, parser::Parser};

    #[test]
    fn empty_imports() {
        let mut arena = AstAllocator::default();
        let result = Parser::parse("".to_string(), &mut arena);

        let deps = Dependencies::get(&result);
        assert!(deps.imports.is_empty());
    }

    #[test]
    fn import_something() {
        let mut arena = AstAllocator::default();
        let result = Parser::parse("import A".to_string(), &mut arena);

        let deps = Dependencies::get(&result);
        assert_eq!(deps.imports.len(), 1);
        assert_eq!(deps.imports[0].path, vec!["A"]);
    }

    #[test]
    fn import_a_bunch() {
        let mut arena = AstAllocator::default();
        let result = Parser::parse("import A\nimport A.B\nimport A.B.C".to_string(), &mut arena);

        let deps = Dependencies::get(&result);
        assert_eq!(deps.imports.len(), 3);
        assert_eq!(deps.imports[0].path, vec!["A"]);
        assert_eq!(deps.imports[1].path, vec!["A", "B"]);
        assert_eq!(deps.imports[2].path, vec!["A", "B", "C"]);
    }
}
