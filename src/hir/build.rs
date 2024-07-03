use std::collections::HashMap;

use index_vec::IndexVec;

use crate::{
    ast,
    span::{spanned_into, Span, Spanned, ToSpanned},
    token::{tokens::Ident, Token},
};

use super::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HirBuildError {
    DuplicatedFuncName(Ident),
    DuplicatedAdtName(Ident),
    VarNotExist(Ident),
    TyNotExist(Ident),
}

#[derive(Debug, Clone, Default)]
pub struct RawProgram {
    adts: IndexVec<AdtId, Spanned<ast::TyDecl>>,
    funcs: IndexVec<UserFuncId, Spanned<ast::Decl>>,
    adt_ids: HashMap<Ident, AdtId>,
    func_ids: HashMap<Ident, FuncId>,
}

impl RawProgram {
    pub fn add_func(
        &mut self,
        Spanned(decl, span): Spanned<ast::Decl>,
    ) -> Result<(), Spanned<HirBuildError>> {
        let Spanned(name, name_span) = decl.name.clone();
        let func_id = self.funcs.push(decl.to_spanned(span));
        let has_dup = self.func_ids.insert(name.clone(), func_id.into()).is_some();
        if has_dup {
            Err(HirBuildError::DuplicatedFuncName(name).to_spanned(name_span))
        } else {
            Ok(())
        }
    }
    pub fn add_adt(
        &mut self,
        Spanned(ty_decl, span): Spanned<ast::TyDecl>,
    ) -> Result<(), Spanned<HirBuildError>> {
        let Spanned(name, name_span) = ty_decl.name.clone();
        let adt_id = self.adts.push(ty_decl.to_spanned(span));
        let has_dup = self.adt_ids.insert(name.clone(), adt_id).is_some();
        if has_dup {
            Err(HirBuildError::DuplicatedAdtName(name).to_spanned(name_span))
        } else {
            Ok(())
        }
    }
    pub fn add_item(
        &mut self,
        Spanned(item, span): Spanned<ast::Item>,
    ) -> Result<(), Spanned<HirBuildError>> {
        match item {
            ast::Item::Decl(decl) => self.add_func(Spanned(decl, span)),
            ast::Item::TyDecl(ty_decl) => self.add_adt(Spanned(ty_decl, span)),
        }
    }
}

pub fn build_hir(raw_program: RawProgram) -> Result<HirProgram, Spanned<HirBuildError>> {
    let mut state = BuilderState {
        adt_ids: raw_program.adt_ids,
        func_ids: raw_program.func_ids,
        item_local: None,
        eq_local: None,
    };
    let user_funcs: IndexVec<UserFuncId, Spanned<UserFunc>> = raw_program
        .funcs
        .into_iter()
        .map(|func| build_func(&mut state, func).map(spanned_into))
        .collect::<Result<_, _>>()?;
    let mut constructor_funcs: IndexVec<ConstructorFuncId, Spanned<ConstructorFunc>> =
        IndexVec::new();
    let adts: IndexVec<AdtId, Spanned<Adt>> = raw_program
        .adts
        .into_iter_enumerated()
        .map(|(adt_id, adt)| build_adt(&mut state, &mut constructor_funcs, adt_id, adt))
        .collect::<Result<_, _>>()?;
    Ok(HirProgram {
        adts,
        user_funcs,
        constructor_funcs,
    })
}

#[derive(Debug, Clone)]
struct BuilderState {
    adt_ids: HashMap<Ident, AdtId>,
    func_ids: HashMap<Ident, FuncId>,
    item_local: Option<ItemLocalState>,
    eq_local: Option<EqLocalState>,
}

#[derive(Debug, Clone, Default)]
struct EqLocalState {
    var_ids: HashMap<Ident, VarId>,
    var_id_counter: usize,
}

#[derive(Default, Debug, Clone)]
struct ItemLocalState {
    tyvars: HashMap<Ident, TyVarId>,
}

fn build_func(
    state: &mut BuilderState,
    Spanned(decl, span): Spanned<ast::Decl>,
) -> Result<Spanned<UserFunc>, Spanned<HirBuildError>> {
    state.item_local = Some(make_item_local_state(decl.quan));
    let ty = build_ty(state, decl.ty)?;
    let equations: Vec<Spanned<Equation>> = match decl.body.into_inner() {
        ast::DeclBody::Inline(_, e, _) => {
            let eq = build_equation(state, e.span(), Vec::new(), e)?;
            vec![eq]
        }
        ast::DeclBody::Equations(eqs) => eqs
            .into_iter()
            .map(|Spanned((_, Spanned(eq, span), _), _)| {
                build_equation(state, span, eq.lhs.into_inner(), eq.rhs)
            })
            .collect::<Result<_, _>>()?,
    };
    let tyvars: Vec<TyVarId> = state
        .item_local
        .take()
        .unwrap()
        .tyvars
        .into_values()
        .collect();
    Ok(UserFunc {
        name: decl.name.into_inner(),
        tyvars,
        ty,
        equations,
    }
    .to_spanned(span))
}

fn make_item_local_state(quan: Option<(Spanned<ast::Quan>, Spanned<Token![.]>)>) -> ItemLocalState {
    match quan {
        Some((Spanned(quan, _), _)) => ItemLocalState {
            tyvars: quan
                .vars
                .into_iter()
                .enumerate()
                .map(|(i, Spanned(x, _))| (x, TyVarId(i)))
                .collect(),
        },
        None => ItemLocalState::default(),
    }
}

#[allow(unreachable_code, unused_variables)]
fn build_adt(
    state: &mut BuilderState,
    constructor_funcs: &mut IndexVec<ConstructorFuncId, Spanned<ConstructorFunc>>,
    adt_id: AdtId,
    Spanned(ty_decl, span): Spanned<ast::TyDecl>,
) -> Result<Spanned<Adt>, Spanned<HirBuildError>> {
    let local_state = make_item_local_state(ty_decl.quan);
    let tyvars: Vec<TyVarId> = local_state.tyvars.iter().map(|(_, &x)| x).collect();
    state.item_local = Some(local_state);
    let this_ty = Ty::Adt(adt_id).to_spanned(span.clone());
    let mut constructors = <Vec<ConstructorFuncId>>::with_capacity(ty_decl.body.len());
    for (i, Spanned((Spanned(ident, _), ty), span)) in
        ty_decl.body.into_inner().into_iter().enumerate()
    {
        let constructor_func_ty = match ty {
            Spanned(Some(ty), span_) => {
                let ty = build_ty(state, ty.to_spanned(span_))?;
                Ty::Func(Box::new(ty.into_inner()), Box::new(this_ty.inner().clone()))
                    .to_spanned(span.clone())
            }
            Spanned(None, _) => this_ty.clone(),
        };
        let consturctor_func = ConstructorFunc {
            name: ident,
            tyvars: tyvars.clone(),
            ty: constructor_func_ty,
            adt: adt_id,
            variant: VariantId(i),
        }
        .to_spanned(span);
        let func_id = constructor_funcs.push(spanned_into(consturctor_func));
        constructors.push(func_id);
    }
    state.item_local = None;
    Ok(Adt {
        name: ty_decl.name.into_inner(),
        tyvars,
        constructors,
    }
    .to_spanned(span))
}

fn build_equation(
    state: &mut BuilderState,
    span: Span,
    lhs: Vec<Spanned<ast::Pat_>>,
    rhs: Spanned<ast::Expr>,
) -> Result<Spanned<Equation>, Spanned<HirBuildError>> {
    state.eq_local = Some(EqLocalState::default());
    let lhs: Vec<Spanned<Pat>> = lhs
        .into_iter()
        .map(|x| build_pat(state, spanned_into(x)))
        .collect::<Result<_, _>>()?;
    let rhs: Spanned<Expr> = build_expr(state, rhs)?;
    Ok(Equation { lhs, rhs }.to_spanned(span))
}

fn build_expr(
    state: &mut BuilderState,
    Spanned(expr, span): Spanned<ast::Expr>,
) -> Result<Spanned<Expr>, Spanned<HirBuildError>> {
    match expr {
        ast::Expr::Var(name) => {
            let var_id = match state.eq_local.as_ref().unwrap().var_ids.get(&name) {
                Some(&x) => x,
                None => return Err(HirBuildError::VarNotExist(name).to_spanned(span)),
            };
            Ok(Expr::Var(var_id).to_spanned(span))
        }
        ast::Expr::Apply(box e0, box e1) => Ok(Expr::Apply(
            Box::new(build_expr(state, e0)?),
            Box::new(build_expr(state, e1)?),
        )
        .to_spanned(span)),
        ast::Expr::InParens(box (_, e, _)) => build_expr(state, e),
        ast::Expr::Tuple((_, es, _)) => {
            let es = es
                .into_inner()
                .into_iter()
                .map(|x| build_expr(state, x))
                .collect::<Result<_, _>>()?;
            Ok(Expr::Tuple(es).to_spanned(span))
        }
    }
}

fn build_pat(
    state: &mut BuilderState,
    Spanned(pat, span): Spanned<ast::Pat>,
) -> Result<Spanned<Pat>, Spanned<HirBuildError>> {
    match pat {
        ast::Pat::Binding(name) => {
            let eq_local = state.eq_local.as_mut().unwrap();
            let var_id = VarId(eq_local.var_id_counter);
            eq_local.var_id_counter += 1;
            eq_local.var_ids.insert(name, var_id);
            Ok(Pat::Binding(var_id).to_spanned(span))
        }
        ast::Pat::Apply(Spanned(ident, ident_span), box p1) => {
            let adt_id = match state.adt_ids.get(&ident) {
                Some(&x) => x,
                None => return Err(HirBuildError::TyNotExist(ident).to_spanned(span)),
            };
            Ok(Pat::Apply(
                adt_id.to_spanned(ident_span),
                Box::new(build_pat(state, p1)?),
            )
            .to_spanned(span))
        }
        ast::Pat::InParens(box (_, p, _)) => build_pat(state, p),
        ast::Pat::Tuple((_, ps, _)) => {
            let ps = ps
                .into_inner()
                .into_iter()
                .map(|x| build_pat(state, x))
                .collect::<Result<_, _>>()?;
            Ok(Pat::Tuple(ps).to_spanned(span))
        }
    }
}

fn build_ty(
    state: &mut BuilderState,
    Spanned(ty, span): Spanned<ast::TyExpr>,
) -> Result<Spanned<Ty>, Spanned<HirBuildError>> {
    match ty {
        ast::TyExpr::Typename(name) => {
            let ty = match state.item_local.as_ref().unwrap().tyvars.get(&name) {
                Some(&tyvar_id) => Ty::TyVar(tyvar_id),
                None => match state.adt_ids.get(&name) {
                    Some(&adt_id) => Ty::Adt(adt_id),
                    None => return Err(HirBuildError::TyNotExist(name).to_spanned(span)),
                },
            };
            Ok(ty.to_spanned(span))
        }
        ast::TyExpr::Apply(box t0, box t1) => Ok(Ty::Apply(
            Box::new(build_ty(state, t0)?.into_inner()),
            Box::new(build_ty(state, t1)?.into_inner()),
        )
        .to_spanned(span)),
        ast::TyExpr::Func(box t0, _, box t1) => Ok(Ty::Func(
            Box::new(build_ty(state, t0)?.into_inner()),
            Box::new(build_ty(state, t1)?.into_inner()),
        )
        .to_spanned(span)),
        ast::TyExpr::InParens(box (_, t, _)) => build_ty(state, t),
        ast::TyExpr::Tuple((_, ts, _)) => {
            let ts = ts
                .into_inner()
                .into_iter()
                .map(|x| build_ty(state, x).map(Spanned::into_inner))
                .collect::<Result<_, _>>()?;
            Ok(Ty::Tuple(ts).to_spanned(span))
        }
    }
}
