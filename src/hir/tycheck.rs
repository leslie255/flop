use std::collections::HashMap;

use crate::span::ToSpanned;

use super::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TycheckError {
    DifferentNumbersOfArgs,
    TooManyArgs,
    InvalidArgPat,
    MismatchedTy { expect: Ty, found: Ty },
}

pub fn tycheck(hir_program: &HirProgram) -> Result<(), Spanned<TycheckError>> {
    let mut cx = Cx::new(hir_program);
    for Spanned(func, _) in &hir_program.user_funcs {
        tycheck_func(&mut cx, func)?;
    }
    Ok(())
}

fn tycheck_func<'p>(cx: &mut Cx<'p>, func: &'p UserFunc) -> Result<(), Spanned<TycheckError>> {
    check_args_count(func)?;
    cx.enter_func(func);
    for Spanned(equ, _) in &func.equations {
        cx.enter_equation(match_bindings(cx, &equ.lhs)?);
        tycheck_expr(cx, &equ.rhs, cx.current_func_cx.as_ref().unwrap().return_ty)?;
        cx.leave_equataion();
    }
    cx.leave_func();
    Ok(())
}

fn tycheck_expr(
    cx: &mut Cx,
    Spanned(expr, span): &Spanned<Expr>,
    expect_ty: &Ty,
) -> Result<(), Spanned<TycheckError>> {
    match expr {
        &Expr::Value(x) => {
            let expr_ty = typeof_value(cx, x);
            if expr_ty == expect_ty {
                Ok(())
            } else {
                Err(TycheckError::MismatchedTy {
                    expect: expect_ty.clone(),
                    found: expr_ty.clone(),
                }
                .to_spanned(span.clone()))
            }
        }
        Expr::Apply(_, _) => todo!(),
        Expr::Tuple(_) => todo!(),
    }
}

fn typeof_value<'p>(cx: &Cx<'p>, value: Value) -> &'p Ty {
    match value {
        Value::Var(x) => cx.current_equ_cx().bindings[&x],
        Value::Func(FuncId::UserFunc(x)) => &cx.program.user_funcs[x].ty,
        Value::Func(FuncId::ConstructorFunc(x)) => &cx.program.constructor_funcs[x].ty,
    }
}

/// A polytype `σ` is a type with quantifiers.
#[derive(Clone, Copy, Debug)]
struct PolyTy<'p> {
    /// Quantifiers.
    quan: &'p [TyVarId],
    mono: &'p Ty,
}

impl<'p> From<&'p Ty> for PolyTy<'p> {
    fn from(mono: &'p Ty) -> Self {
        Self { quan: &[], mono }
    }
}

/// The type context Γ.
/// Doesn't actually store all `x: τ` pairs, because a complete list of such pairs only exist
/// as a mental model.
/// It instead stores necessary information about (the program's global items, local bindings,
/// etc.), such that it is able to say `x : τ` is true or false, so it essentially behaves as
/// if such list exists.
#[derive(Clone, Debug)]
pub struct Cx<'p> {
    program: &'p HirProgram,
    current_func_cx: Option<FuncLocalCx<'p>>,
    current_equ_cx: Option<EquLocalCx<'p>>,
}

impl<'p> Cx<'p> {
    const fn new(program: &'p HirProgram) -> Self {
        Self {
            program,
            current_func_cx: None,
            current_equ_cx: None,
        }
    }
    /// Panics if already in a function.
    /// Panics if function has less than one equations.
    fn enter_func(&mut self, func: &'p UserFunc) {
        assert!(self.current_func_cx.is_none());
        let sig = PolyTy {
            quan: &func.tyvars,
            mono: &func.ty,
        };
        let first_equ = func
            .equations
            .first()
            .unwrap_or_else(|| panic!("Function must have at least one equation"));
        let return_ty = func_return_ty(sig, first_equ.lhs.len());
        self.current_func_cx = Some(FuncLocalCx { sig, return_ty });
    }
    /// Panics if wasn't in a function.
    fn leave_func(&mut self) {
        assert!(self.current_func_cx.is_some());
        self.current_func_cx = None;
    }
    /// Panics if already in a function.
    fn enter_equation(&mut self, bindings: HashMap<VarId, &'p Ty>) {
        assert!(self.current_equ_cx.is_none());
        self.current_equ_cx = Some(EquLocalCx { bindings });
    }
    /// Panics if wasn't in a function.
    fn leave_equataion(&mut self) {
        assert!(self.current_equ_cx.is_some());
        self.current_equ_cx = None;
    }

    /// Panics if not in a function.
    fn current_func_cx(&self) -> &FuncLocalCx<'p> {
        self.current_func_cx.as_ref().unwrap()
    }

    /// Panics if not in an equation.
    fn current_equ_cx(&self) -> &EquLocalCx<'p> {
        self.current_equ_cx.as_ref().unwrap()
    }
}

#[derive(Clone, Debug)]
struct FuncLocalCx<'p> {
    sig: PolyTy<'p>,
    return_ty: &'p Ty,
}

#[derive(Clone, Debug)]
struct EquLocalCx<'p> {
    bindings: HashMap<VarId, &'p Ty>,
}

/// Assumes `check_args_count` is already performed on the current function.
/// Panics if `check_args_count` is not performed and encounters invalid number of arguments.
fn match_bindings<'p>(
    cx: &Cx<'p>,
    pats: &'p [Spanned<Pat>],
) -> Result<HashMap<VarId, &'p Ty>, Spanned<TycheckError>> {
    let mut ty = cx.current_func_cx().sig.mono;
    let mut pairs = <HashMap<VarId, &'p Ty>>::new();
    for pat in pats {
        let arg_ty = match ty {
            Ty::Func(x, y) => {
                ty = y;
                x
            }
            _ => return Ok(pairs),
        };
        match_pat_ty(&mut pairs, pat, arg_ty)?;
    }
    Ok(pairs)
}

/// Panics if `arg_count` is less than maximum number of arguments possible.
fn func_return_ty(func_ty: PolyTy, arg_count: usize) -> &Ty {
    let mut ty = func_ty.mono;
    for _ in 0..arg_count {
        match ty {
            Ty::Func(_, y) => {
                ty = y;
            }
            _ => panic!(),
        }
    }
    ty
}

/// Match a pat with a type, collects produced bindings `x: τ` to `acc`.
fn match_pat_ty<'p>(
    acc: &mut HashMap<VarId, &'p Ty>,
    Spanned(pat, span): &'p Spanned<Pat>,
    ty: &'p Ty,
) -> Result<(), Spanned<TycheckError>> {
    match (pat, ty) {
        (&Pat::Binding(x), t) => {
            acc.insert(x, t);
            Ok(())
        }
        (_pat, Ty::Apply(_, _)) => todo!("Complex patterns"),
        (Pat::Apply(_adt, box _p1), Ty::Adt(_)) => todo!("Complex patterns"),
        (Pat::Apply(..), _) => Err(TycheckError::InvalidArgPat.to_spanned(span.clone())),
        (Pat::Tuple(_), Ty::Tuple(_)) => todo!("Complex patterns"),
        (Pat::Tuple(..), _) => Err(TycheckError::InvalidArgPat.to_spanned(span.clone())),
    }
}

/// Make sure all equations have equal number of argument patterns.
fn check_args_count(func: &UserFunc) -> Result<(), Spanned<TycheckError>> {
    let max_args: usize = {
        let mut n = 0;
        let mut ty = func.ty.inner();
        while let Ty::Func(_, x) = ty {
            ty = x.as_ref();
            n += 1;
        }
        n
    };
    if let [x, ys @ ..] = &func.equations[..] {
        let n = x.lhs.len();
        if n > max_args {
            return Err(TycheckError::TooManyArgs.to_spanned(x.span()));
        }
        for Spanned(y, span) in ys {
            if y.lhs.len() != n {
                return Err(TycheckError::DifferentNumbersOfArgs.to_spanned(span.clone()));
            }
        }
    }
    Ok(())
}
