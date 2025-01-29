use peregrine::syn::ast::{Ast, Decl, DeclId, Import};

pub struct Dependencies<'ast> {
    pub imports: Vec<(DeclId, &'ast Import)>,
}

impl<'ast> Dependencies<'ast> {
    fn new(imports: Vec<(DeclId, &'ast Import)>) -> Dependencies<'ast> {
        Dependencies { imports }
    }

    pub fn get(ast: &'ast Ast) -> Dependencies<'ast> {
        let mut vec = Vec::default();

        for &decl in ast.decls() {
            if let Decl::Import(import) = ast.get_decl(decl) {
                vec.push((decl, import))
            } else {
                break;
            }
        }

        Dependencies::new(vec)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use peregrine::syn;

    #[test]
    fn empty_imports() {
        let result = syn::parse("");

        let deps = Dependencies::get(&result);
        assert!(deps.imports.is_empty());
    }

    #[test]
    fn import_something() {
        let result = syn::parse("import A");

        let deps = Dependencies::get(&result);
        assert_eq!(deps.imports.len(), 1);
        assert_eq!(deps.imports[0].1.path, vec!["A"]);
    }

    #[test]
    fn import_a_bunch() {
        let result = syn::parse("import A\nimport A.B\nimport A.B.C");

        let deps = Dependencies::get(&result);
        assert_eq!(deps.imports.len(), 3);
        assert_eq!(deps.imports[0].1.path, vec!["A"]);
        assert_eq!(deps.imports[1].1.path, vec!["A", "B"]);
        assert_eq!(deps.imports[2].1.path, vec!["A", "B", "C"]);
    }
}
