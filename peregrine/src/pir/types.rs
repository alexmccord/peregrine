use std::collections::HashMap;

// Yes.
//
// In the very first line of writing the type system, you can
// already see an opportunity to encode Girard's paradox by
// exploiting integer overflow here.
//
// I do not care. It's a bootstrapping compiler.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Level(u128);

impl Level {
    fn successor(self) -> Level {
        Level(self.0 + 1)
    }
}

#[derive(Debug)]
pub enum Universe {
    Uni(Level), // proof relevant
    Omega,      // proof irrelevant
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Var(u64);

#[derive(Debug)]
pub struct Context {
    // TODO: This is probably wrong, it only allows us to map
    // from variables to terms, but perhaps terms can map to
    // terms also? e.g. `x : A` is a term that's bound to `s`,
    // hence the notation `x : A : s`.
    bindings: HashMap<Var, TermId>,
}

impl Context {
    fn new() -> Context {
        Context {
            bindings: HashMap::default(),
        }
    }

    fn insert(&mut self, var: Var, t: TermId) {
        self.bindings.insert(var, t);
    }

    fn lookup(&self, var: &Var) -> Option<&TermId> {
        self.bindings.get(var)
    }
}

type TermId = id_arena::Id<Term>;

pub struct TermArena {
    arena: id_arena::Arena<Term>,
}

impl TermArena {
    pub fn new() -> TermArena {
        TermArena {
            arena: id_arena::Arena::default(),
        }
    }

    pub fn alloc(&mut self, term: Term) -> TermId {
        self.arena.alloc(term)
    }

    pub fn get(&self, id: TermId) -> &Term {
        &self.arena[id]
    }
}

#[derive(Debug)]
pub struct Binding(Var, TermId);

#[derive(Debug)]
pub struct Sort(Universe);

#[derive(Debug)]
pub enum Term {
    // x
    Var(Var),
    // s
    Sort(Sort),
    // \(x : A). t
    Lam(Binding, TermId),
    // t u
    App(TermId, TermId),
    // Pi s,s' (x : A). B
    Pi(Sort, Sort, Binding, TermId),
    // !-elim(A, t)
    BottomElim(TermId, TermId),
    // !
    Bottom,
    // t ~A u
    Equiv(TermId, TermId, TermId),
    // refl(t)
    Refl(TermId),
    // transp(A, t, B, u, t', e)
    Transp(TermId, TermId, TermId, TermId, TermId, TermId),
    // cast(A, B, e, t)
    Cast(TermId, TermId, TermId, TermId),
    //
    Pi1,
    Pi2,
    OmegaExt,
    PiExt,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypingError {
    UnknownVar(Var),
}

pub fn check(ctx: &Context, term: &Term) -> Result<(), TypingError> {
    match term {
        Term::Var(var) => {
            let res = ctx.lookup(&var).ok_or(TypingError::UnknownVar(*var))?;
            Ok(()) // TODO: Conv typing rule
        }
        Term::Sort(sort) => todo!(),
        Term::Lam(binding, cell) => todo!(),
        Term::App(cell, cell1) => todo!(),
        Term::Pi(level, sort, binding, cell) => todo!(),
        Term::BottomElim(cell, cell1) => todo!(),
        Term::Bottom => todo!(),
        Term::Equiv(cell, cell1, cell2) => todo!(),
        Term::Refl(cell) => todo!(),
        Term::Transp(cell, cell1, cell2, cell3, cell4, cell5) => todo!(),
        Term::Cast(cell, cell1, cell2, cell3) => todo!(),
        Term::Pi1 => todo!(),
        Term::Pi2 => todo!(),
        Term::OmegaExt => todo!(),
        Term::PiExt => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_var_in_context() {
        let mut arena = TermArena::new();
        let mut ctx = Context::new();

        let a_ty = arena.alloc(Term::Sort(Sort(Universe::Uni(Level(0)))));

        let x = Var(0);
        let x_term = arena.alloc(Term::Var(x));

        ctx.insert(x, a_ty);

        let res = check(&ctx, arena.get(x_term));
        assert_eq!(res, Ok(()));
    }

    #[test]
    fn check_var_not_in_context() {
        let mut arena = TermArena::new();
        let ctx = Context::new();

        let x = Var(0);
        let x_term = arena.alloc(Term::Var(x));

        let res = check(&ctx, arena.get(x_term));
        assert_eq!(res, Err(TypingError::UnknownVar(x)));
    }
}
