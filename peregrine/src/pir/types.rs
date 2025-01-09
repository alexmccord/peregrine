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
pub enum Relevance {
    Universe, // proof relevant
    Omega,    // proof irrelevant
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Var(u64);

#[derive(Debug)]
pub struct Context {
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
pub struct Sort(Relevance, Level);

#[derive(Debug)]
pub enum Term {
    // x
    Var(Var),
    // s_i
    Sort(Sort),
    // \(x : A). t
    Lam(Binding, TermId),
    // t u
    App(TermId, TermId),
    // Pi j s,i (x : A). B
    Pi(Level, Sort, Binding, TermId),
    // (t, u)
    Pair(TermId, TermId),
    // fst(t)
    Fst(TermId),
    // snd(t)
    Snd(TermId),
    // exists j i (x : A). B
    Exists(Level, Level, Binding, TermId),
    // !-elim(A, t)
    BottomElim(TermId, TermId),
    // !
    Bottom,
    // *
    Star,
    // ()
    Top,
    // t ~A u
    Equiv(TermId, TermId, TermId),
    // refl(t)
    Refl(TermId),
    // transp(t, B, u, t', e)
    Transp(TermId, TermId, TermId, TermId, TermId),
    // cast(A, B, e, t)
    Cast(TermId, TermId, TermId, TermId),
    // castrefl(A, t)
    CastRefl(TermId, TermId),
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
        Term::Pair(cell, cell1) => todo!(),
        Term::Fst(cell) => todo!(),
        Term::Snd(cell) => todo!(),
        Term::Exists(level, level1, binding, cell) => todo!(),
        Term::BottomElim(cell, cell1) => todo!(),
        Term::Bottom => todo!(),
        Term::Star => todo!(),
        Term::Top => todo!(),
        Term::Equiv(cell, cell1, cell2) => todo!(),
        Term::Refl(cell) => todo!(),
        Term::Transp(cell, cell1, cell2, cell3, cell4) => todo!(),
        Term::Cast(cell, cell1, cell2, cell3) => todo!(),
        Term::CastRefl(cell, cell1) => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_var_in_context() {
        let mut arena = TermArena::new();
        let top_term = arena.alloc(Term::Top);
        let var_0 = Var(0);
        let var_0_term = arena.alloc(Term::Var(var_0));

        let mut ctx = Context::new();
        ctx.insert(var_0, top_term);

        let res = check(&ctx, arena.get(var_0_term));
        assert_eq!(res, Ok(()));
    }

    #[test]
    fn check_var_not_in_context() {
        let mut arena = TermArena::new();
        let var_0 = Var(0);
        let var_0_term = arena.alloc(Term::Var(var_0));

        let ctx = Context::new();

        let res = check(&ctx, arena.get(var_0_term));
        assert_eq!(res, Err(TypingError::UnknownVar(var_0)));
    }
}
