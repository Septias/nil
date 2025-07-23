use super::union_find::UnionFind;
use super::{known, AttrSource};
use crate::def::{
    BindingValue, Bindings, Expr, ExprId, Literal, NameId, NameResolution, ResolveResult,
};
use crate::{FileId, Module};
use la_arena::ArenaMap;
use salsa::Database;
use smol_str::SmolStr;
use std::collections::btree_map::{BTreeMap, Entry};
use std::sync::Arc;
use syntax::ast::{BinaryOpKind, UnaryOpKind};

impl AttrSource {
    fn unify(&mut self, rhs: Self) {
        if *self == Self::Unknown {
            *self = rhs;
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TyVar(u32);

/// A type used in inference.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Ty {
    Unknown,

    Bool,
    Int,
    Float,
    String,
    Path,

    List(TyVar),
    Lambda(TyVar, TyVar),
    Attrset(Attrset),

    External(super::Ty),
}

impl Ty {
    fn intern(self, ctx: &mut InferCtx<'_>) -> TyVar {
        TyVar(ctx.table.push(self))
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
struct Attrset {
    /// Map from names to types.
    fields: BTreeMap<SmolStr, (TyVar, AttrSource)>,
    dyn_ty: Option<TyVar>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InferenceResult {
    name_ty_map: ArenaMap<NameId, super::Ty>,
    expr_ty_map: ArenaMap<ExprId, super::Ty>,
}

impl InferenceResult {
    pub fn ty_for_name(&self, name: NameId) -> super::Ty {
        self.name_ty_map[name].clone()
    }

    pub fn ty_for_expr(&self, expr: ExprId) -> super::Ty {
        self.expr_ty_map[expr].clone()
    }
}

pub(crate) fn infer_query(
    db: &dyn Database,
    file: FileId,
    module: Module,
    nameres: NameResolution,
) -> Arc<InferenceResult> {
    infer_with(db, file, module, nameres)
}

pub(crate) fn infer_with(
    db: &dyn Database,
    file: FileId,
    module: Module,
    nameres: NameResolution,
) -> Arc<InferenceResult> {
    let module = db.module(file);
    let nameres = db.name_resolution(file);
    let table = UnionFind::new(module.names().len() + module.exprs().len(), |_| Ty::Unknown);
    let mut ctx = InferCtx {
        module: &module,
        nameres: &nameres,
    };
    let ty = ctx.infer_expr(module.entry_expr());
    Arc::new(ctx.finish())
}

struct InferCtx<'db> {
    module: &'db Module,
    nameres: &'db NameResolution,
}

impl InferCtx<'_> {
    fn new_ty_var(&mut self) -> TyVar {
        TyVar(self.table.push(Ty::Unknown))
    }

    fn ty_for_name(&self, i: NameId) -> TyVar {
        TyVar(u32::from(i.into_raw()))
    }

    fn ty_for_expr(&self, i: ExprId) -> TyVar {
        TyVar(self.module.names().len() as u32 + u32::from(i.into_raw()))
    }

    fn import_external(&mut self, ty: super::Ty) -> TyVar {
        let ty = match ty {
            super::Ty::Unknown => Ty::Unknown,
            super::Ty::Bool => Ty::Bool,
            super::Ty::Int => Ty::Int,
            super::Ty::Float => Ty::Float,
            super::Ty::String => Ty::String,
            super::Ty::Path => Ty::Path,
            super::Ty::List(_) | super::Ty::Lambda(..) | super::Ty::Attrset(_) => Ty::External(ty),
        };
        TyVar(self.table.push(ty))
    }

    fn infer_expr(&mut self, e: ExprId) -> TyVar {
        let ty = self.infer_expr_inner(e);
        let placeholder_ty = self.ty_for_expr(e);
        self.unify_var(placeholder_ty, ty);
        ty
    }

    fn infer_expr_inner(&mut self, e: ExprId) -> TyVar {
        match &self.module[e] {
            Expr::Missing => todo!(),
            Expr::Reference(smol_str) => todo!(),
            Expr::Literal(literal) => todo!(),
            Expr::Lambda(idx, pat, idx1) => todo!(),
            Expr::With(idx, idx1) => todo!(),
            Expr::Assert(idx, idx1) => todo!(),
            Expr::IfThenElse(idx, idx1, idx2) => todo!(),
            Expr::Binary(binary_op_kind, idx, idx1) => todo!(),
            Expr::Apply(idx, idx1) => todo!(),
            Expr::Unary(unary_op_kind, idx) => todo!(),
            Expr::HasAttr(idx, items) => todo!(),
            Expr::Select(idx, items, idx1) => todo!(),
            Expr::StringInterpolation(items) => todo!(),
            Expr::PathInterpolation(items) => todo!(),
            Expr::List(items) => todo!(),
            Expr::LetIn(bindings, idx) => todo!(),
            Expr::Attrset(bindings) => todo!(),
            Expr::LetAttrset(bindings) => todo!(),
            Expr::RecAttrset(bindings) => todo!(),
            Expr::CurPos => todo!(),
        }
    }

    /// Infer the type of a binding by joining inherits and statics into field values.
    fn infer_bindings(&mut self, bindings: &Bindings) -> Attrset {
        todo!()
    }

    /// `field` is `None` for dynamic fields.
    fn infer_set_field(&mut self, set_ty: TyVar, field: Option<SmolStr>, src: AttrSource) -> TyVar {
        todo!()
    }

    fn unify_var_ty(&mut self, var: TyVar, rhs: Ty) {
        unimplemented!()
    }

    fn unify_var(&mut self, lhs: TyVar, rhs: TyVar) {
        unimplemented!()
    }

    fn unify(&mut self, lhs: Ty, rhs: Ty) -> Ty {
        unimplemented!()
    }

    fn finish(mut self) -> InferenceResult {
        unimplemented!()
    }
}
