use std::collections::{HashMap, VecDeque};

use crate::syntax::ast::{AstId, Data, Decl, Expr, ExprId, Let, Program, Struct};

// Similar to attribute grammars, e.g. in this fragment:
//
//  let id x = x
//
// We have this AST:
//
//  Program [
//    Decl::Let(Let::DeclExpr(
//      Expr::App(Expr::Var(Symbol("id")), Expr::Var(Symbol("x"))),
//      Expr::Var(Symbol("x")),
//    ))
//  ]
//
// The problem is that this doesn't give us a way to know the
// context that an instance of the AST is embedded within. For
// instance, these questions are all valid:
//
// 1. For any expression, is it intensional, i.e. left side of `=`
//    for the purpose of deciding whether an expression invokes the
//    elimination rule or the introduction rule for pattern matching.
// 2. For any expression, is it extensional, i.e. right side of `=`?
// 3. For any expression, is it an expression or a type in `e : T`?
// 4. For any expression, in which scope does it belong to for the
//    purpose of resolving free vs bound variables?
//
// To solve these kinds of problems, `ProgramSem` maps each AST node
// to their semantic interpretation. As a direct consequence, we can
//
// 1. Compute semantic information exactly once and reuse it across
//    all passes over the AST.
// 2. Passes over the AST can be done in constant stack, preventing
//    us from running afoul of stack overflows.
// 3. Guarantee consistency of AST node interpretation, eliminating
//    entire classes of bugs.
// 4. Make it easy to add new semantic interpretation of AST nodes,
//    without the headache of plumbing such interpretation over
//    existing passes.
//
// Using the same example as in the beginning, we might annotate as
//
//  `id` in the left side of `=` as intensional.
//  `x` in the left side of `=` as intensional.
//  `x` in the right side of `=` as extensional.
//
// N.B. As far as I can tell, there's no reason to apply the same
// on `Decl`s, because they don't have this same expression problem.
#[derive(Debug)]
pub enum Sem {
    Intensional(ExprId),
    Extensional(ExprId),
}

impl Sem {
    pub fn is_intensional_of(&self, id: ExprId) -> bool {
        match self {
            Sem::Intensional(e) => e == &id,
            _ => false,
        }
    }

    pub fn is_extensional_of(&self, id: ExprId) -> bool {
        match self {
            Sem::Extensional(e) => e == &id,
            _ => false,
        }
    }
}

pub struct ProgramSem {
    semantics: HashMap<ExprId, Sem>,
    antecedents: HashMap<AstId, AstId>,
}

impl ProgramSem {
    pub fn build(program: &Program) -> ProgramSem {
        ProgramSemBuilder::new(program).build()
    }

    pub fn get_sem(&self, id: ExprId) -> &Sem {
        &self.semantics[&id]
    }

    pub fn get_antecedent(&self, id: impl Into<AstId>) -> Option<AstId> {
        self.antecedents.get(&id.into()).cloned()
    }
}

struct ProgramSemBuilder<'ast> {
    program: &'ast Program,
    semantics: HashMap<ExprId, Sem>,
    antecedents: HashMap<AstId, AstId>,
    queue: VecDeque<AstId>,
}

impl<'ast> ProgramSemBuilder<'ast> {
    fn new(program: &'ast Program) -> ProgramSemBuilder<'ast> {
        ProgramSemBuilder {
            program,
            semantics: HashMap::default(),
            antecedents: HashMap::default(),
            queue: VecDeque::new(),
        }
    }

    fn add_edge(&mut self, antecedent: impl Into<AstId>, child: impl Into<AstId>) {
        let child = child.into();
        self.antecedents.insert(child, antecedent.into());
        self.queue.push_back(child);
    }

    fn build(mut self) -> ProgramSem {
        for decl in self.program.decls() {
            match self.program.get_decl(*decl) {
                Decl::Module(_) => (),
                Decl::Import(_) => (),
                Decl::Struct(Struct(sig)) => self.add_edge(decl, sig),
                Decl::Data(Data(sig)) => self.add_edge(decl, sig),
                Decl::Let(l) => match l {
                    Let::Decl(e1) => self.add_edge(decl, e1),
                    Let::DeclExpr(e1, e2) => {
                        self.add_edge(decl, e1);
                        self.add_edge(e1, e2);
                    }
                    Let::DeclExprIn(e1, e2, e3) => todo!(),
                },
                Decl::Error(_, Some(e)) => self.add_edge(decl, e),
                Decl::Error(_, None) => (),
            }
        }

        while let Some(ast) = self.queue.pop_front() {
            match ast {
                AstId::ExprId(expr_id) => match self.program.get_expr(expr_id) {
                    Expr::Var(_) => (),
                    Expr::Lam(x, e) => {
                        self.add_edge(ast, x);
                        self.add_edge(x, e);
                    }
                    Expr::App(f, e) => {
                        self.add_edge(ast, f);
                        self.add_edge(f, e);
                    }
                    Expr::Ann(e, t) => todo!(),
                    Expr::Num(_) => (),
                    Expr::Error(_, Some(expr_id)) => todo!(),
                    Expr::Error(_, None) => (),
                },
                AstId::DeclId(decl_id) => match self.program.get_decl(decl_id) {
                    Decl::Module(module) => (),
                    Decl::Import(import) => (),
                    Decl::Struct(_) => todo!(),
                    Decl::Data(data) => todo!(),
                    Decl::Let(l) => match l {
                        Let::Decl(expr_id) => todo!(),
                        Let::DeclExpr(expr_id, expr_id1) => todo!(),
                        Let::DeclExprIn(expr_id, expr_id1, expr_id2) => todo!(),
                    },
                    Decl::Error(_, Some(decl_id)) => todo!(),
                    Decl::Error(_, None) => (),
                },
            }
        }

        ProgramSem {
            semantics: self.semantics,
            antecedents: self.antecedents,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        semantics::sem::ProgramSem,
        syntax::ast::{AstAllocator, Decl, Expr, Let, Program, Symbol},
    };

    #[test]
    fn let_id_x_be_x() {
        let mut arena = AstAllocator::default();
        let mut decls = Vec::new();

        let id_int = arena.alloc_expr(Expr::Var(Symbol("id".to_string()))); // ExprId 0
        let x_int = arena.alloc_expr(Expr::Var(Symbol("x".to_string()))); // ExprId 1
        let id_x_int = arena.alloc_expr(Expr::App(id_int, x_int)); // ExprId 2
        let x_ext = arena.alloc_expr(Expr::Var(Symbol("x".to_string()))); // ExprId 3
        let decl = arena.alloc_decl(Decl::Let(Let::DeclExpr(id_x_int, x_ext))); // DeclId 0
        decls.push(decl);

        let program = Program::new(arena, decls);
        let sem = ProgramSem::build(&program);

        // TODO: we don't bind anything to a Sem value yet.
        // assert!(sem.get_sem(id_int).is_intensional_of(id_int));
        // assert!(sem.get_sem(x_int).is_intensional_of(x_int));
        // assert!(sem.get_sem(id_x_int).is_intensional_of(id_x_int));
        // assert!(sem.get_sem(x_ext).is_extensional_of(x_ext));

        assert!(sem.get_antecedent(id_int).is_some_and(|e| e == id_x_int));
        assert!(sem.get_antecedent(x_int).is_some_and(|e| e == id_int));
        assert!(sem.get_antecedent(id_x_int).is_some_and(|e| e == decl));
        assert!(sem.get_antecedent(x_ext).is_some_and(|e| e == id_x_int));
        assert!(sem.get_antecedent(decl).is_none());
    }
}
