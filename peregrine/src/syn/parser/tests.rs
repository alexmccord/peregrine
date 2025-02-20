#![cfg(test)]
use crate::ast::decl;
use crate::ast::decl::Path;
use crate::ast::decl::PathNode;
use crate::ast::expr;
use crate::ast::Position;
use crate::syn;

#[test]
fn parse_nothing() {
    let result = syn::parse("");
    assert!(result.ast().decls().is_empty());
}

#[test]
fn parse_module_decl() {
    let result = syn::parse("module A.B.C");
    let mut iter = result.ast().decls().iter();

    let module_decl = iter.next().unwrap();
    let module = module_decl.as_module().unwrap();
    assert_eq!(
        module.path(),
        Some(&Path::new(vec![
            PathNode::Name("A".to_string()),
            PathNode::Name("B".to_string()),
            PathNode::Name("C".to_string()),
        ]))
    );

    assert!(iter.next().is_none());
}

#[test]
fn parse_import_decl() {
    let result = syn::parse("import A.B.C");
    let source_module = &result.source_module;
    let mut iter = result.ast().decls().iter();

    let import_decl = iter.next().unwrap();
    let import = import_decl.as_import().unwrap();
    assert_eq!(
        import.path(),
        &Path::new(vec![
            PathNode::Name("A".to_string()),
            PathNode::Name("B".to_string()),
            PathNode::Name("C".to_string()),
        ])
    );

    let import_src_span = source_module.source_span(import_decl);
    assert_eq!(import_src_span.begin, Position::new(1, 0));
    assert_eq!(import_src_span.end, Position::new(1, 12));

    assert!(iter.next().is_none());
}

#[test]
fn parse_export_decl() {
    let result = syn::parse("export let five = 5");
    let mut iter = result.ast().decls().iter();

    let export_decl = iter.next().unwrap();
    let export = export_decl.as_export().unwrap();
    let let_decl = export.decl().as_let().unwrap();

    match let_decl {
        decl::Let::Decl(..) => panic!("not this one"),
        decl::Let::DeclExpr(f, e) => {
            let expr::Var(var) = f.as_var().unwrap();
            assert_eq!(var, "five");

            let expr::Num(num) = e.as_num().unwrap();
            assert_eq!(num, "5");
        }
    }
}

#[test]
fn parse_let_five_be_5() {
    let result = syn::parse("let five = 5");
    let mut iter = result.ast().decls().iter();

    let let_decl = iter.next().unwrap();
    let l = let_decl.as_let().unwrap();

    match l {
        decl::Let::Decl(..) => panic!("not this one"),
        decl::Let::DeclExpr(f, e) => {
            let expr::Var(var) = f.as_var().unwrap();
            assert_eq!(var, "five");

            let expr::Num(num) = e.as_num().unwrap();
            assert_eq!(num, "5");
        }
    }
}

#[test]
fn parse_let_paren_x_paren_be_2() {
    let result = syn::parse("let (x) = 2");
    let mut iter = result.ast().decls().iter();

    let let_decl = iter.next().unwrap();
    let l = let_decl.as_let().unwrap();

    let f = l.get_the_expr();
    let expr::Var(var) = f.as_var().unwrap();
    assert_eq!(var, "x");

    let e = l.get_be_expr().unwrap();
    let expr::Num(num) = e.as_num().unwrap();
    assert_eq!(num, "2");
}

#[test]
fn parse_let_id_which_is_a_to_a() {
    let result = syn::parse("let id : a -> a");
    let mut iter = result.ast().decls().iter();

    let let_decl = iter.next().unwrap();
    let l = let_decl.as_let().unwrap();

    match l {
        decl::Let::DeclExpr(..) => panic!("not this one"),
        decl::Let::Decl(f) => {
            let expr::Ann(e, t) = f.as_ann().unwrap();
            let expr::Var(var) = e.as_var().unwrap();
            assert_eq!(var, "id");

            let app_2 = t.as_app().unwrap();
            let expr::Var(arg_a_2) = app_2.argument().as_var().unwrap();
            assert_eq!(arg_a_2, "a");

            let app_1 = app_2.function().as_app().unwrap();
            let expr::Var(arg_a_1) = app_1.argument().as_var().unwrap();
            assert_eq!(arg_a_1, "a");

            let expr::Var(var_arrow) = app_1.function().as_var().unwrap();
            assert_eq!(var_arrow, "->");
        }
    }
}

#[test]
fn parse_let_id_x_be_x() {
    let result = syn::parse("let id x = x");
    let mut iter = result.ast().decls().iter();

    let let_decl = iter.next().unwrap();
    let l = let_decl.as_let().unwrap();

    match l {
        decl::Let::Decl(..) => panic!("not this one"),
        decl::Let::DeclExpr(f_id, e_id) => {
            let expr::App(f, e) = f_id.as_app().unwrap();
            let expr::Var(id) = f.as_var().unwrap();
            let expr::Var(x) = e_id.as_var().unwrap();
            assert_eq!(id, "id");
            assert_eq!(x, "x");

            let expr::Var(x) = e.as_var().unwrap();
            assert_eq!(x, "2");
        }
    }
}

#[test]
fn parse_let_five_of_missing_type() {
    let result = syn::parse("let five :");
    let errors = result.errors.clone();
    let mut iter = result.ast().decls().iter();

    let let_decl = iter.next().unwrap();
    let l = let_decl.as_let().unwrap();

    match l {
        decl::Let::DeclExpr(..) => panic!("not this one"),
        decl::Let::Decl(f) => {
            let expr::Ann(e, t) = f.as_ann().unwrap();
            let expr::Var(five) = e.as_var().unwrap();
            assert_eq!(five, "five");
            assert_eq!(t.as_error().unwrap(), None);

            let error = errors
                .iter()
                .find_map(|(id, e)| (id == &t.id()).then_some(e))
                .unwrap();

            assert_eq!(error, &syn::SyntaxError::MissingExpr);
        }
    }
}

#[test]
fn parse_unit() {
    let result = syn::parse("let unit = ()");
    let mut iter = result.ast().decls().iter();

    let let_decl = iter.next().unwrap();
    let l = let_decl.as_let().unwrap();

    match l {
        decl::Let::Decl(_) => panic!("not this one"),
        decl::Let::DeclExpr(_, e) => assert!(e.is_unit()),
    }
}

#[test]
fn parsing_preserves_spans() {
    let result = syn::parse("let five = 5");
    let mut iter = result.ast().decls().iter();

    let let_decl = iter.next().unwrap();
    let let_decl_span = result.source_module.source_span(let_decl);

    assert_eq!(let_decl_span.begin, Position::new(1, 0));
    assert_eq!(let_decl_span.end, Position::new(1, 12));
}
