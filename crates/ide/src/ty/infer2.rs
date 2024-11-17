use super::union_find::UnionFind;
use super::{known, AttrSource, TyDatabase};
use crate::def::{
    BindingValue, Bindings, Expr, ExprId, Literal, NameId, NameResolution, ResolveResult,
};
use crate::{FileId, Module};
use la_arena::ArenaMap;
use smol_str::SmolStr;
use std::collections::btree_map::{BTreeMap, Entry};
use std::mem;
use std::sync::Arc;
use syntax::ast::{BinaryOpKind, UnaryOpKind};

pub(crate) fn infer_query(db: &dyn TyDatabase, file: FileId) -> Arc<InferenceResult> {
    let expect_ty = db.module_expected_ty(file);
    infer_with(db, file, expect_ty)
}

pub(crate) fn infer_with(
    db: &dyn TyDatabase,
    file: FileId,
    expect_ty: Option<super::Ty>,
) -> Arc<InferenceResult> {
    let module = db.module(file);
    let nameres = db.name_resolution(file);
    let table = UnionFind::new(module.names().len() + module.exprs().len(), |_| Ty::Unknown);
    let mut ctx = InferCtx {
        module: &module,
        nameres: &nameres,
        table,
    };
    let ty = ctx.infer_expr(module.entry_expr());
    if let Some(expect_ty) = expect_ty {
        ctx.unify_var_ty(ty, Ty::External(expect_ty));
    }
    Arc::new(ctx.finish())
}
