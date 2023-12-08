use std::{
    collections::HashSet,
    fmt::{self, Debug},
    rc::Rc,
};

use index_vec::IndexVec;

use crate::{
    ast::{AstNode, VarId},
    symbol::GlobalSymbols,
    ty::{FnType, Ty},
    utils::{FmtEnumerated, IndexVecUncheckedGet},
};

#[derive(Debug, Clone)]
pub enum Item {
    LocalFunction {
        name: Rc<str>,
        /// `VarId`s of the arguments.
        arg_vars: Vec<VarId>,
        sig: FnType,
        body: Vec<AstNode>,
        /// Types of all variables in this function.
        var_tys: IndexVec<VarId, Ty>,
    },
    ExternFunction {
        name: Rc<str>,
        sig: FnType,
    },
}

/// Either a known type or a type variable.
/// Cloning a `MaybeUnknownTy` is cheap due to use of `Rc` in `Ty` and `TyVar` being `Copy`able.
#[derive(Clone)]
enum MaybeUnknownTy {
    Known(Ty),
    UnKnown(TyVar),
}
impl Debug for MaybeUnknownTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Known(ty) => Debug::fmt(ty, f),
            Self::UnKnown(tyvar) => write!(f, "{{{:?}}}", tyvar),
        }
    }
}
impl From<Ty> for MaybeUnknownTy {
    fn from(value: Ty) -> Self {
        Self::Known(value)
    }
}
impl From<TyVar> for MaybeUnknownTy {
    fn from(value: TyVar) -> Self {
        Self::UnKnown(value)
    }
}
impl MaybeUnknownTy {
    /// Panics if is unknown.
    #[track_caller]
    pub fn expect_known(self, reason: impl AsRef<str>) -> Ty {
        match self {
            MaybeUnknownTy::Known(ty) => ty,
            MaybeUnknownTy::UnKnown(..) => {
                panic!(
                    "Called `MaybeUnknownTy::expect_known` on an unknown type (reason: {})",
                    reason.as_ref()
                )
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct TyVar(usize);
impl Debug for TyVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "tyvar{}", self.0)
    }
}
impl index_vec::Idx for TyVar {
    fn from_usize(idx: usize) -> Self {
        Self(idx)
    }
    fn index(self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone)]
enum TyConstraint {
    Eq(TyVar, MaybeUnknownTy),
    /// Constraints that are no longer effective.
    /// For removing constraints from an `IndexVec`.
    Ineffective,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ConstraintId(usize);
impl Debug for ConstraintId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "constraint{}", self.0)
    }
}
impl index_vec::Idx for ConstraintId {
    fn from_usize(idx: usize) -> Self {
        Self(idx)
    }
    fn index(self) -> usize {
        self.0
    }
}

/// Manages local type inference/check.
#[derive(Clone)]
struct LocalContext {
    /// Name of this function, for debug and error reporting.
    fn_name: Rc<str>,
    /// The canonical array of all the type variables in the local context.
    /// Maps the type variables to all constraints that relates to it (appears on either the LHS or RHS).
    tyvars: IndexVec<TyVar, HashSet<ConstraintId>>,
    /// The constriants.
    constraints: IndexVec<ConstraintId, TyConstraint>,
    /// Type (known or unknown) of the variables in this function.
    var_tys: IndexVec<VarId, MaybeUnknownTy>,
}

impl Debug for LocalContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LocalContext")
            .field("tyvars", &FmtEnumerated(&self.tyvars))
            .field("constraints", &FmtEnumerated(&self.constraints))
            .field("var_tys", &FmtEnumerated(&self.var_tys))
            .finish()
    }
}

impl LocalContext {
    fn from_sig(fn_name: Rc<str>, arg_vars: &[VarId], sig: &FnType) -> Self {
        let mut var_tys = IndexVec::<VarId, MaybeUnknownTy>::new();
        for (i, &arg_var) in arg_vars.iter().enumerate() {
            let ty = Rc::clone(&sig.inputs[i]);
            debug_assert!(
                var_tys.push(MaybeUnknownTy::Known(ty)) == arg_var,
                "argument variables of a function isn't continuous (arg_vars: {arg_vars:?}, sig: {sig:?})"
            );
        }
        Self {
            fn_name,
            tyvars: IndexVec::new(),
            constraints: IndexVec::new(),
            var_tys,
        }
    }
    fn new_tyvar(&mut self) -> TyVar {
        self.tyvars.push(HashSet::new())
    }
    fn add_constraint(&mut self, constraint: TyConstraint) -> ConstraintId {
        let constraint_id = self.constraints.push(constraint);
        let constraint = unsafe { self.constraints.get_unchecked(constraint_id) };
        match constraint {
            TyConstraint::Eq(lhs, rhs) => {
                self.tyvars[*lhs].insert(constraint_id);
                match rhs {
                    MaybeUnknownTy::Known(..) => {}
                    &MaybeUnknownTy::UnKnown(rhs) => {
                        self.tyvars[rhs].insert(constraint_id);
                    }
                }
            }
            TyConstraint::Ineffective => {}
        }
        constraint_id
    }
    /// Remove a constraint by setting it to `Ineffective`.
    /// Also removes it from all the related constraints of type variables.
    /// Returns `Err` if the constraint was already removed before.
    fn try_remove_constraint(&mut self, id: ConstraintId) -> Result<(), ()> {
        let constraint = &mut self.constraints[id];
        match constraint {
            TyConstraint::Eq(lhs, rhs) => {
                self.tyvars[*lhs].remove(&id);
                match rhs {
                    MaybeUnknownTy::Known(..) => {}
                    &mut MaybeUnknownTy::UnKnown(rhs) => {
                        self.tyvars[rhs].remove(&id);
                    }
                }
            }
            TyConstraint::Ineffective => return Err(()),
        }
        *constraint = TyConstraint::Ineffective;
        Ok(())
    }
    /// Remove a constraint by setting it to `Ineffective`.
    /// Also removes it from all the related constraints of type variables.
    /// Panics if the constraint has already been removed.
    fn remove_constraint(&mut self, id: ConstraintId) {
        self.try_remove_constraint(id).unwrap()
    }
    /// Returns all constraints a type variable is related to.
    fn related_constraints(&self, tyvar: TyVar) -> &HashSet<ConstraintId> {
        &self.tyvars[tyvar]
    }
    /// Variables must be added in order, otherwise panics.
    #[cfg_attr(debug_assertions, track_caller)]
    #[allow(unused_variables)] // `var_id` isn't used in release builds
    fn add_var(&mut self, var_id: VarId, ty: MaybeUnknownTy) {
        let new_id = self.var_tys.push(ty);
        // Reason for debug assert instead of assert:
        // IndexVec will panic anyways so no UB. So this is here only for better panic message.
        debug_assert_eq!(var_id, new_id);
    }
}

/// Returns `None` if the statement is considered unobservable.
/// For this reason, misusing `cook_stmt` and `cook_expr` is very dangerous.
fn cook_stmt(global: &GlobalSymbols, local: &mut LocalContext, stmt: AstNode) -> Option<AstNode> {
    match stmt {
        AstNode::Variable(..) => None,
        AstNode::InvalidVariable(ident) => {
            panic!("Invalid variable {:?} in function {:?}", ident, local);
        }
        AstNode::IntLiteral(..) => None,
        AstNode::FloatLiteral(..) => None,
        AstNode::VarDecl(var_id, Some(ty), rhs) => {
            local.add_var(var_id, ty.into());
            let _rhs = rhs?; // if no rhs statement is unobservable
            todo!("expression parsing");
        }
        AstNode::VarDecl(var_id, None, rhs) => {
            let rhs = rhs.unwrap_or_else(|| {
                panic!(
                    "cannot have type and rhs be both absent in `let` (in function {:?}).",
                    local.fn_name.as_ref()
                )
            });
            let tyvar = local.new_tyvar();
            local.add_var(var_id, tyvar.into());
            match *rhs {
                AstNode::Variable(var_id) => {
                    let rhs_ty = local.var_tys[var_id].clone();
                    local.add_constraint(TyConstraint::Eq(tyvar, rhs_ty));
                    None
                }
                _ => todo!(),
            }
        }
        AstNode::BinaryOp(_, _, _) => todo!(),
        AstNode::UnaryOp(_, _) => todo!(),
        AstNode::Assign(_, _) => todo!(),
        AstNode::TupleLiteral(_) => todo!(),
        AstNode::FnDef { .. } => panic!("nested function definition not allowed"),
        AstNode::ExternFn(_, _) => todo!(),
        AstNode::Return(_) => todo!(),
        AstNode::IfElse(_, _, _) => todo!(),
        AstNode::Call(_, _) => todo!(),
    }
}

fn cook_function(
    global: &GlobalSymbols,
    name: Rc<str>,
    arg_vars: Vec<VarId>,
    sig: FnType,
    body: Vec<AstNode>,
) -> Item {
    let mut local = LocalContext::from_sig(Rc::clone(&name), &arg_vars, &sig);
    let body: Vec<AstNode> = body
        .into_iter()
        .filter_map(|stmt| cook_stmt(global, &mut local, stmt))
        .collect();
    dbg!(&local);
    dbg!(&body);
    let var_tys = local
        .var_tys
        .into_iter()
        .map(|ty| {
            ty.expect_known(format!(
                "unable to infer the type of variable in function {:?}",
                name
            ))
        })
        .collect();
    Item::LocalFunction {
        name,
        arg_vars,
        sig,
        body,
        var_tys,
    }
}

pub fn cook_ast(ast: Vec<AstNode>, global: &GlobalSymbols) -> Vec<Item> {
    ast.into_iter()
        .map(|root_node| match root_node {
            AstNode::FnDef {
                name,
                arg_vars,
                sig,
                body,
            } => cook_function(global, name, arg_vars, sig, body),
            AstNode::ExternFn(_, _) => todo!(),
            _ => unreachable!(), // should've handled this during global symbol building.
        })
        .collect()
}
