use crate::ast::decl::DeclId;
use crate::ast::expr::ExprId;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum NodeId {
    ExprId(ExprId),
    DeclId(DeclId),
}

// Peregrine is gonna have transitive type classes. -_-
impl From<ExprId> for NodeId {
    fn from(value: ExprId) -> Self {
        NodeId::ExprId(value)
    }
}

impl From<DeclId> for NodeId {
    fn from(value: DeclId) -> Self {
        NodeId::DeclId(value)
    }
}

// Rust's "automatic Deref" can bite me.
impl From<&ExprId> for NodeId {
    fn from(value: &ExprId) -> Self {
        value.into()
    }
}

// Rust's "automatic Deref" can bite me.
impl From<&DeclId> for NodeId {
    fn from(value: &DeclId) -> Self {
        value.into()
    }
}

impl PartialEq<ExprId> for NodeId {
    fn eq(&self, other: &ExprId) -> bool {
        match (self, other) {
            (Self::ExprId(lhs), rhs) => lhs == rhs,
            _ => false,
        }
    }
}

impl PartialEq<DeclId> for NodeId {
    fn eq(&self, other: &DeclId) -> bool {
        match (self, other) {
            (Self::DeclId(lhs), rhs) => lhs == rhs,
            _ => false,
        }
    }
}

impl PartialEq<NodeId> for ExprId {
    fn eq(&self, other: &NodeId) -> bool {
        other == self
    }
}

impl PartialEq<NodeId> for DeclId {
    fn eq(&self, other: &NodeId) -> bool {
        other == self
    }
}
