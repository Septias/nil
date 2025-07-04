use super::union_find::UnionFind;
use super::{AttrSource, known};
use crate::def::{
    BindingValue, Bindings, Expr, ExprId, Literal, NameId, NameResolution, ResolveResult,
};
use crate::{FileId, Module, ModuleSourceMap};
use la_arena::ArenaMap;
use salsa::Database;
use smol_str::SmolStr;
use std::cell::RefCell;
use std::collections::btree_map::{BTreeMap, Entry};
use std::collections::{HashMap, HashSet};
use std::mem;
use std::rc::Rc;
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

/// A single identifier.
/// The name should be a debrujin index and the path is used for set accesses.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Var {
    pub level: usize,
    pub id: usize,
    pub lower_bounds: Rc<RefCell<Vec<Ty>>>,
    pub upper_bounds: Rc<RefCell<Vec<Ty>>>,
}

#[derive(Debug, Clone)]
pub struct PolymorphicType {
    pub body: Ty,
    pub level: usize,
}

impl PolymorphicType {
    pub fn new(body: Ty, level: usize) -> Self {
        Self { body, level }
    }
}

/// A type used in inference.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Ty {
    Top,
    Bottom,
    Unknown,

    Bool,
    String,
    Number,
    Path,
    Null,
    Undefined,

    Var(Var),
    Function(Box<Ty>, Box<Ty>),
    List(Vec<Ty>),
    Record(HashMap<String, Ty>),
    Optional(Box<Ty>),
    Pattern(HashMap<String, (Ty, Option<Ty>)>, bool),

    // Complex Tys only created by simplification
    Union(Box<Ty>, Box<Ty>),
    Inter(Box<Ty>, Box<Ty>),
    Recursive(Var, Box<Ty>),
}

impl std::hash::Hash for Ty {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        todo!()
    }
}

impl Ty {
    fn intern(self, ctx: &mut InferCtx<'_>) -> TyVar {
        TyVar(ctx.table.push(self))
    }
}

/// A nix attrset.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
struct Attrset {
    /// Map from names to types.
    fields: BTreeMap<SmolStr, (TyVar, AttrSource)>,
    // This is the type for all non-static fields.
    // Is this really the same as `super::Attrset::rest`?
    dyn_ty: Option<TyVar>,
}

/// Result from inference.
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

/// Infer a type for a DB-file.
pub(crate) fn infer_query(db: &dyn Database, file: FileId) -> Arc<InferenceResult> {
    let expect_ty = db.module_expected_ty(file);
    infer_with(db, file, expect_ty)
}

/// Type inference with expected type.
pub(crate) fn infer_with(
    db: &dyn Database,
    file: FileId,
    db: &dyn TyDatabase,
    file_id: FileId,
    expect_ty: Option<super::Ty>,
) -> Arc<InferenceResult> {
    let module = db.module(file_id);
    let nameres = db.name_resolution(file_id);
    let source_map = db.source_map(file_id);
    let table = UnionFind::new(module.names().len() + module.exprs().len(), |_| Ty::Unknown);
    let mut ctx = InferCtx {
        module: &module,
        nameres: &nameres,
        source_map: &source_map,
        with: vec![],
    };
    Arc::new(ctx.finish())
}

struct InferCtx<'db> {
    module: &'db Module,
    nameres: &'db NameResolution,
    source_map: &'db ModuleSourceMap,
    with: Vec<Ty>,
}

impl InferCtx<'_> {
    fn new_ty_var(&mut self) -> TyVar {
        TyVar(self.table.push(Ty::Unknown))
    }

    /// Get the type for a given [NameId].
    fn ty_for_name(&self, i: NameId) -> TyVar {
        TyVar(u32::from(i.into_raw()))
    }

    /// Get the type for a given ExprId.
    fn ty_for_expr(&self, i: ExprId) -> TyVar {
        TyVar(self.module.names().len() as u32 + u32::from(i.into_raw()))
    }

    /// Import an external type from [super::Ty].
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

    /// Infer the type of an expression.
    fn infer_expr(&mut self, e: ExprId) -> Ty {
        let ty = self.type_term(e, 0);
        ty
    }

    pub fn constrain(&mut self, lhs: &Ty, rhs: &Ty) {
        self.constrain_inner(lhs, rhs, &mut HashSet::new())
    }

    fn constrain_inner<'a>(&mut self, lhs: &'a Ty, rhs: &'a Ty, cache: &mut HashSet<(Ty, Ty)>) {
        if lhs == rhs {
            return;
        }
        let lhs_rhs = (lhs.clone(), rhs.clone());

        match (lhs, rhs) {
            (Ty::Var(..), _) | (_, Ty::Var(..)) => {
                if cache.contains(&lhs_rhs) {
                    return;
                }
                cache.insert(lhs_rhs.clone());
            }
            _ => (),
        }

        match (lhs, rhs) {
            (Ty::Bool, Ty::Bool)
            | (Ty::Number, Ty::Number)
            | (Ty::String, Ty::String)
            | (Ty::Path, Ty::Path)
            | (Ty::Null, Ty::Null)
            | (Ty::Undefined, _) => (),

            (Ty::Function(l0, r0), Ty::Function(l1, r1)) => {
                self.constrain_inner(l1, l0, cache);
                self.constrain_inner(r0, r1, cache);
            }

            (Ty::Record(fs0), Ty::Record(fs1)) => {
                for (n1, t1) in fs1 {
                    match fs0.iter().find(|(n0, _)| *n0 == n1) {
                        Some((_, t0)) => self.constrain_inner(t0, t1, cache),
                        None => {
                            todo!()
                        }
                    }
                }
            }

            (Ty::Record(rcd), Ty::Pattern(pat, wildcart)) => {
                if *wildcart {
                    for (pat, (t1, optional)) in pat.iter() {
                        match rcd.iter().find(|(n0, _)| *n0 == pat) {
                            Some((_, t0)) => self.constrain_inner(t0, t1, cache),
                            None => {
                                if optional.is_none() {
                                    // return Err(InferError::MissingRecordField {
                                    //     field: pat.clone(),
                                    // });
                                    todo!()
                                }
                            }
                        }
                    }
                } else {
                    let mut keys = rcd.iter().map(|(n, _)| n).collect::<HashSet<_>>();
                    for (n1, (t1, optional)) in pat.iter() {
                        match rcd.iter().find(|(n0, _)| *n0 == n1) {
                            Some((_, t0)) => {
                                if let Some(opt) = optional {
                                    self.constrain_inner(t0, opt, cache);
                                }
                                self.constrain_inner(t0, t1, cache);
                                keys.remove(n1);
                            }
                            None => {
                                if optional.is_none() {
                                    // return Err(InferError::MissingRecordField {
                                    //     field: n1.clone(),
                                    // });
                                    todo!()
                                }
                            }
                        }
                    }
                    if !keys.is_empty() {
                        return Err(InferError::TooManyField {
                            field: keys.into_iter().next().unwrap().clone(),
                        });
                    }
                }
            }

            (Ty::Optional(o1), Ty::Optional(o0)) => {
                constrain_inner(context, o0, o1, cache)?;
            }

            (Ty::List(ls1), Ty::List(ls2)) if ls1.len() == 1 && ls2.len() == 1 => {
                constrain_inner(context, &ls1[0], &ls2[0], cache)?;
            }

            // application
            // function constraints
            // selection
            (Ty::Var(lhs), rhs) if rhs.level() <= lhs.level => {
                lhs.upper_bounds.borrow_mut().push(rhs.clone());
                for lower_bound in lhs.lower_bounds.borrow().iter() {
                    constrain_inner(context, lower_bound, rhs, cache)?;
                }
            }

            (Ty::Pattern(pat, _), rhs @ Ty::Var(_)) => {
                constrain_inner(
                    context,
                    &Ty::Record(
                        pat.clone()
                            .into_iter()
                            .map(|(name, (_ty, opt))| (name, opt.unwrap_or(Ty::Undefined)))
                            .collect(),
                    ),
                    rhs,
                    cache,
                )?;
            }

            // let-binding
            // record typing
            (lhs, Ty::Var(rhs)) if lhs.level() <= rhs.level => {
                rhs.lower_bounds.borrow_mut().push(lhs.clone());
                for upper_bound in rhs.upper_bounds.borrow().iter() {
                    constrain_inner(context, lhs, upper_bound, cache)?;
                }
            }
            (Ty::Var(_), rhs) => {
                let rhs_extruded = extrude(context, rhs, false, lhs.level(), &mut HashMap::new());
                constrain_inner(context, lhs, &rhs_extruded, cache)?;
            }
            (lhs, Ty::Var(_)) => {
                let lhs_extruded = extrude(context, lhs, true, rhs.level(), &mut HashMap::new());
                constrain_inner(context, &lhs_extruded, rhs, cache)?;
            }

            _ => {
                return Err(InferError::CannotConstrain {
                    lhs: lhs.clone(),
                    rhs: rhs.clone(),
                })
            }
        }

        Ok(())
    }

    /// Subroutine of [infer_expr].
    fn type_term(&mut self, e: ExprId, lvl: usize) -> Ty {
        use Ty::*;
        match self.module[e] {
            // TODO: why does oxcalica not use `Indentifier`?
            // Expr::Identifier(super::ast::Identifier { name, span, .. }) => {
            //     if let Some(var) = self.lookup(name) {
            //         return Ok(var.instantiate(self, lvl));
            //     }

            //     // Handle with statement which could be used to supply vars
            //     if let Some(with) = &self.with {
            //         if let ty @ Ty::Var(var) = with {
            //             if let Some(rec) = var.as_record() {
            //                 if let Some(ty) = rec.get(name).cloned() {
            //                     return Ok(ty);
            //                 }
            //             } else {
            //                 let res = Ty::Var(self.fresh_var(lvl));
            //                 constrain(
            //                     self,
            //                     ty,
            //                     &Ty::Record([(name.to_string(), res.clone())].into()),
            //                 )
            //                 .map_err(|e| e.span(span))?;
            //                 return Ok(res);
            //             }
            //         }
            //     }

            //     Err(InferError::UnknownIdentifier.span(span))
            // }
            Expr::Unary { ops, .. } => self.type_term(ops, lvl),

            Expr::Binary { op, lhs, rhs } => {
                match op {
                    /* // TODO: HasAttribute is an Expr for oxcalica, why?
                    Application is an Expr for oxcalica
                    /* BinaryOpKind::HasAttribute => {
                        let ty1 = type_term(self, lhs, lvl)?;
                        let name = rhs
                            .as_identifier_str()
                            .map_err(|e| e.span(rhs.get_span()))?;
                        if let Ty::Var(_) = &ty1 {
                            constrain(
                                self,
                                &ty1,
                                &Record([(name, Ty::Optional(Box::new(Ty::Undefined)))].into()), */
                            )
                            .map_err(|e| e.span(lhs.get_span()))?;
                        };
                        return Ok(Bool);
                    } */


                    // TODO: Attribute selection is an expr for oxcalica, why?
                    /* BinaryOpKind::AttributeSelection => {
                        let ty = type_term(self, lhs, lvl)?;
                        let name = rhs
                            .as_identifier_str()
                            .map_err(|e| e.span(rhs.get_span()))?;

                        return match ty {
                            Ty::Var(_) => {
                                let res = Ty::Var(self.fresh_var(lvl));
                                constrain(self, &ty, &Ty::Record([(name, res.clone())].into()))
                                    .map_err(|e| e.span(lhs.get_span()))?;
                                Ok(res)
                            }
                            Record(rc) => {
                                let name = rhs
                                    .as_identifier_str()
                                    .map_err(|e| e.span(rhs.get_span()))?;
                                if let Some(ty) = rc.get(&name) {
                                    Ok(ty.clone())
                                } else {
                                    Err(SpannedError {
                                        error: InferError::MissingRecordField { field: name },
                                        span: span.clone(),
                                    })
                                }
                            }
                            _ => Err(SpannedError {
                                error: InferError::TypeMismatch {
                                    expected: TypeName::Record,
                                    found: ty.get_name(),
                                },
                                span: lhs.get_span().clone(),
                            }),
                        };
                    } */
                    _ => (),
                }
                let ty1 = self.type_term(lhs, lvl);
                let ty2 = self.type_term(rhs, lvl);

                match op {
                    // Application etc.
                    // Application is an Expr for oxcalica
                    /* BinaryOpKind::Application => {
                        let res = Ty::Var(self.fresh_var(lvl));
                        constrain(
                            self,
                            &ty1,
                            &Ty::Function(Box::new(ty2), Box::new(res.clone())),
                        )
                        .map_err(|e| e.span(lhs.get_span()))?;
                        Ok(res)
                    } */
                    // Object modifications
                    BinaryOpKind::Concat => match (&ty1, &ty2) {
                        (Ty::List(l), Ty::List(l2)) => Ty::List([l.clone(), l2.clone()].concat()),
                        (var1 @ Ty::Var(v1), var2 @ Ty::Var(v2)) => {
                            self.constrain(var1, &Ty::List(vec![]));
                            self.constrain(self, var2, &Ty::List(vec![]))
                                .map_err(|e| e.span(rhs.get_span()))?;
                            Ty::List(
                                [
                                    v1.as_list().unwrap_or_default(),
                                    v2.as_list().unwrap_or_default(),
                                ]
                                .concat(),
                            )
                        }

                        (Ty::List(l), var @ Ty::Var(_)) | (var @ Ty::Var(_), Ty::List(l)) => {
                            constrain(self, var, &Ty::List(vec![]))
                                .map_err(|e| e.span(rhs.get_span()))?;
                            Ty::List(l.clone())
                        }
                        (Ty::List(_), ty2) => todo!(), //TODO: add err here
                        (ty1, _) => Err(SpannedError {
                            error: InferError::TypeMismatch {
                                expected: TypeName::List,
                                found: ty1.get_name(),
                            },
                            span: lhs.get_span().clone(),
                        }),
                    },

                    BinaryOpKind::Update => match (&ty1, &ty2) {
                        (Ty::Record(rc1), Ty::Record(rc2)) => {
                            let mut rc = rc1.clone();
                            rc.extend(rc2.clone());
                            Ok(Ty::Record(rc))
                        }

                        (Ty::Var(v1), Ty::Var(v2)) => {
                            constrain(self, &ty1, &Ty::Record(HashMap::new()))
                                .map_err(|e| e.span(lhs.get_span()))?;
                            constrain(self, &ty2, &Ty::Record(HashMap::new()))
                                .map_err(|e| e.span(rhs.get_span()))?;

                            let mut rc1 = v1.as_record().unwrap_or_default();
                            rc1.extend(v2.as_record().unwrap_or_default());
                            Ok(Ty::Record(rc1))
                        }

                        (Ty::Record(rc1), var @ Ty::Var(_))
                        | (var @ Ty::Var(_), Ty::Record(rc1)) => {
                            constrain(self, var, &Ty::Record(HashMap::new()))
                                .map_err(|e| e.span(rhs.get_span()))?;
                            Ok(Ty::Record(rc1.clone()))
                        }
                        (Ty::Record(_), ty2) => Err(SpannedError {
                            error: InferError::TypeMismatch {
                                expected: TypeName::Record,
                                found: ty2.get_name(),
                            },
                            span: rhs.get_span().clone(),
                        }),

                        (ty1, _) => Err(SpannedError {
                            error: InferError::TypeMismatch {
                                expected: TypeName::Record,
                                found: ty1.get_name(),
                            },
                            span: lhs.get_span().clone(),
                        }),
                    },

                    // Primitives
                    BinaryOpKind::Mul | BinaryOpKind::Div | BinaryOpKind::Sub => {
                        constrain(self, &ty1, &Ty::Number).map_err(|e| e.span(lhs.get_span()))?;
                        constrain(self, &ty2, &Ty::Number).map_err(|e| e.span(rhs.get_span()))?;
                        Ok(Ty::Number)
                    }
                    BinaryOpKind::Add => {
                        match (&ty1, &ty2) {
                            (Ty::Number, Ty::Number) => Ok(Ty::Number),
                            (Ty::String | Ty::Path, Ty::String | Ty::Path) => Ok(Ty::String),
                            (Ty::Var(v1), Ty::Var(v2)) => {
                                // TODO: is this correct?
                                v1.lower_bounds.borrow_mut().extend([
                                    Ty::Number,
                                    Ty::String,
                                    Ty::Path,
                                ]);

                                v2.lower_bounds.borrow_mut().extend([
                                    Ty::Number,
                                    Ty::String,
                                    Ty::Path,
                                ]);

                                Ok(Ty::Union(
                                    Box::new(Ty::Number),
                                    Box::new(Ty::Union(Box::new(Ty::String), Box::new(Ty::Path))),
                                ))
                            }

                            (var @ Ty::Var(_), Ty::Number) | (Ty::Number, var @ Ty::Var(_)) => {
                                constrain(self, var, &Ty::Number)
                                    .map_err(|e| e.span(lhs.get_span()))?;
                                Ok(Ty::Number)
                            }
                            (var @ Ty::Var(_), Ty::String) | (Ty::String, var @ Ty::Var(_)) => {
                                constrain(self, var, &Ty::String)
                                    .map_err(|e| e.span(lhs.get_span()))?;
                                Ok(Ty::String)
                            }
                            (var @ Ty::Var(_), Ty::Path) | (Ty::Path, var @ Ty::Var(_)) => {
                                constrain(self, var, &Ty::Path)
                                    .map_err(|e| e.span(lhs.get_span()))?;
                                Ok(Ty::Path)
                            }
                            (Ty::Number | Ty::Path | Ty::String, ty2) => Err(SpannedError {
                                error: InferError::TypeMismatch {
                                    // TODO: this should be union of Number, String, Path
                                    expected: TypeName::Number,
                                    found: ty2.get_name(),
                                },
                                span: rhs.get_span().clone(),
                            }),
                            (ty1, _) => Err(SpannedError {
                                error: InferError::TypeMismatch {
                                    // TODO: this should be union of Number, String, Path
                                    expected: TypeName::Number,
                                    found: ty1.get_name(),
                                },
                                span: lhs.get_span().clone(),
                            }),
                        }
                    }

                    // Misc
                    BinaryOpKind::AttributeFallback => {
                        constrain(self, &ty1, &ty2).map_err(|e| e.span(lhs.get_span()))?;
                        constrain(self, &ty1, &ty2).map_err(|e| e.span(rhs.get_span()))?;
                        Ok(Ty::Union(Box::new(ty1), Box::new(ty2)))
                    }

                    // Comparisons
                    BinaryOpKind::LessThan
                    | BinaryOpKind::LessThanEqual
                    | BinaryOpKind::GreaterThan
                    | BinaryOpKind::GreaterThanEqual
                    | BinaryOpKind::Equal
                    | BinaryOpKind::NotEqual => match (&ty1, &ty2) {
                        (ty @ Ty::Var(_), _) | (_, ty @ Ty::Var(_)) => {
                            constrain(self, ty, &ty2).map_err(|e| e.span(lhs.get_span()))?;
                            Ok(Bool)
                        }
                        (ty1, ty2) if ty1 != ty2 => Err(SpannedError {
                            error: InferError::TypeMismatch {
                                expected: ty1.get_name(),
                                found: ty2.get_name(),
                            },
                            span: rhs.get_span().clone(),
                        }),
                        _ => Ok(Bool),
                    },

                    // Logical oprators
                    BinaryOpKind::And | BinaryOpKind::Or | BinaryOpKind::Implication => {
                        constrain(self, &ty1, &Ty::Bool).map_err(|e| e.span(lhs.get_span()))?;
                        constrain(self, &ty2, &Ty::Bool).map_err(|e| e.span(rhs.get_span()))?;
                        Ok(Bool)
                    }
                    _ => panic!("unimplemented binary operator: {:?}", op),
                }
            }

            // Language constructs
            Expr::Attrset(Bindings) => {
                let Bindings {
                    attrs,
                    inherit,
                    is_recursive,
                } = todo!();
                if *is_recursive {
                    let vars: Vec<_> = attrs
                        .iter()
                        .map(|(ident, _expr)| {
                            (
                                ident.name.to_string(),
                                ContextType::Type(Ty::Var(self.fresh_var(lvl))),
                            )
                        })
                        .collect();

                    let mut inherits = load_inherit(self, span.clone(), lvl, inherit)?;
                    inherits.extend(vars.clone());

                    self.with_scope(inherits, |ctx| {
                        let names = vars.into_iter().map(|(name, var)| {
                            (name, var.into_type().unwrap().into_var().unwrap())
                        });
                        let expressions = attrs.iter().map(|(_, expr)| expr);

                        let (ok, errs): (Vec<_>, Vec<_>) = names
                            .into_iter()
                            .zip(expressions)
                            .map(move |((name, e_ty), rhs)| {
                                let ty = ctx.with_scope(
                                    vec![(
                                        name.to_string(),
                                        ContextType::Type(Ty::Var(e_ty.clone())),
                                    )],
                                    |ctx| type_term(ctx, rhs, lvl + 1),
                                )?;
                                constrain(ctx, &ty, &Ty::Var(e_ty.clone()))
                                    .map_err(|e| e.span(rhs.get_span()))?;
                                Ok((name, Ty::Var(e_ty)))
                            })
                            .partition_map(|r| match r {
                                Ok(v) => Either::Left(v),
                                Err(v) => Either::Right(v),
                            });

                        if errs.is_empty() {
                            Ok(Ty::Record(ok.into_iter().collect()))
                        } else {
                            Err(SpannedError {
                                error: InferError::MultipleErrors(errs),
                                span: span.clone(),
                            })
                        }
                    })
                } else {
                    let mut vars: HashMap<_, _> = attrs
                        .iter()
                        .map(|(ident, expr)| {
                            (ident.name.to_string(), type_term(self, expr, lvl).unwrap())
                        })
                        .collect();
                    let ok = load_inherit(self, span.clone(), lvl, inherit);
                    vars.extend(
                        ok?.into_iter()
                            .map(|(name, ty)| (name.to_string(), ty.instantiate(self, lvl))),
                    );
                    Ok(Record(vars))
                }
            }

            Expr::LetIn {
                bindings,
                inherit,
                body,
                span,
            } => {
                let binds: Vec<_> = bindings
                    .iter()
                    .map(|(name, _)| {
                        (
                            name.name.to_string(),
                            ContextType::Type(Ty::Var(self.fresh_var(lvl + 1))),
                        )
                    })
                    .collect();

                let mut inherits = load_inherit(self, span.clone(), lvl, inherit)?;
                inherits.extend(binds.clone());
                let (ok, err): (Vec<_>, Vec<_>) = self
                    .with_scope(inherits, |ctx| {
                        let names = binds.into_iter().map(|(name, var)| {
                            (name, var.into_type().unwrap().into_var().unwrap())
                        });
                        let expressions = bindings.iter().map(|(_, expr)| expr);

                        names
                            .into_iter()
                            .zip(expressions)
                            .map(move |((name, e_ty), rhs)| {
                                let ty = ctx.with_scope(
                                    vec![(
                                        name.to_string(),
                                        ContextType::Type(Ty::Var(e_ty.clone())),
                                    )],
                                    |ctx| type_term(ctx, rhs, lvl + 1),
                                )?;
                                let bind = bindings.iter().find(|(n, _)| n.name == *name).unwrap();
                                bind.0.var.set(coalesc_type(ctx, &ty)).unwrap();
                                constrain(ctx, &ty, &Ty::Var(e_ty.clone()))
                                    .map_err(|e| e.span(rhs.get_span()))?;
                                Ok((
                                    name,
                                    ContextType::PolymorhicType(PolymorphicType::new(
                                        Ty::Var(e_ty),
                                        lvl,
                                    )),
                                ))
                            })
                            .collect_vec()
                    })
                    .into_iter()
                    .partition_map(|r| match r {
                        Ok(v) => Either::Left(v),
                        Err(v) => Either::Right(v),
                    });

                if !err.is_empty() {
                    return Err(SpannedError {
                        error: InferError::MultipleErrors(err),
                        span: span.clone(),
                    });
                }

                let ret = self.with_scope(ok, |ctx| type_term(ctx, body, lvl))?;
                Ok(ret)
            }

            Expr::Lambda {
                pattern,
                body,
                span,
            } => {
                let mut added = vec![];
                let ty = match pattern {
                    crate::ast::Pattern::Record {
                        patterns,
                        is_wildcard,
                        name,
                    } => {
                        let mut item = vec![];

                        for pattern in patterns {
                            match pattern {
                                PatternElement::Identifier(ident) => {
                                    let var = Ty::Var(self.fresh_var(lvl));
                                    item.push((ident.name.clone(), (var.clone(), None)));
                                    added.push((ident.name.to_string(), ContextType::Type(var)));
                                }
                                PatternElement::DefaultIdentifier(name, expr) => {
                                    let ty = type_term(self, expr, lvl)?;
                                    let var = Ty::Var(self.fresh_var(lvl));
                                    constrain(self, &var, &ty).map_err(|e| e.span(span))?;

                                    item.push((name.name.clone(), (var.clone(), Some(ty))));
                                    added.push((name.name.to_string(), ContextType::Type(var)));
                                }
                            }
                        }

                        let ty = Ty::Pattern(item.clone().into_iter().collect(), *is_wildcard);
                        if let Some(name) = name {
                            let var = self.fresh_var(lvl);
                            if *is_wildcard {
                                constrain(
                                    self,
                                    &Ty::Var(var.clone()),
                                    &Ty::Record(
                                        item.into_iter()
                                            .map(|(name, (var, _))| (name, var))
                                            .collect(),
                                    ),
                                )
                                .map_err(|e| e.span(span))?;
                            } else {
                                constrain(self, &Ty::Var(var.clone()), &ty)
                                    .map_err(|e| e.span(span))?;
                            }
                            added.push((name.to_string(), ContextType::Type(Ty::Var(var))));
                        }
                        ty
                    }
                    crate::ast::Pattern::Identifier(Identifier { name, .. }) => {
                        let ty = Ty::Var(self.fresh_var(lvl));
                        added.push((name.to_string(), ContextType::Type(ty.clone())));
                        ty
                    }
                };
                let ret = self.with_scope(added, |context| type_term(context, body, lvl))?;
                Ok(Function(Box::new(ty), Box::new(ret)))
            }

            Ast::With { set, body, span } => {
                let ty = type_term(self, set, lvl)?;
                match ty {
                    var @ Ty::Var(_) => {
                        self.set_with(var);
                        let ret = type_term(self, body, lvl);
                        self.remove_with();
                        ret
                    }
                    Ty::Record(rc) => self.with_scope(
                        rc.iter()
                            .filter(|(name, _)| self.with.is_some() || self.lookup(name).is_none())
                            .map(|(name, ty)| (name.to_string(), ContextType::Type(ty.clone())))
                            .collect(),
                        |ctx| type_term(ctx, body, lvl),
                    ),
                    _ => Err(SpannedError {
                        error: InferError::TypeMismatch {
                            expected: TypeName::Record,
                            found: ty.get_name(),
                        },
                        span: span.clone(),
                    }),
                }
            }

            Expr::Conditional {
                condition,
                expr1,
                expr2,
                span,
            } => {
                let ty = type_term(self, condition, lvl)?;
                match ty {
                    Ty::Var(_) => {
                        constrain(self, &ty, &Ty::Bool)
                            .map_err(|e| e.span(condition.get_span()))?;
                    }
                    Ty::Bool => (),
                    _ => {
                        return Err(SpannedError {
                            error: InferError::TypeMismatch {
                                expected: TypeName::Bool,
                                found: ty.get_name(),
                            },
                            span: span.clone(),
                        })
                    }
                }
                let ty1 = type_term(self, expr1, lvl)?;
                let ty2 = type_term(self, expr2, lvl)?;
                if ty1 != ty2 {
                    Ok(Union(Box::new(ty1), Box::new(ty2)))
                } else {
                    Ok(ty1)
                }
            }
            Expr::Assert {
                condition,
                expr,
                span: _,
            } => {
                let ty = type_term(self, condition, lvl)?;
                match ty {
                    Ty::Bool => (),
                    Ty::Var(_) => {
                        constrain(self, &ty, &Ty::Bool)
                            .map_err(|e| e.span(condition.get_span()))?;
                    }
                    _ => {
                        return Err(SpannedError {
                            error: InferError::TypeMismatch {
                                expected: TypeName::Bool,
                                found: ty.get_name(),
                            },
                            span: condition.get_span().clone(),
                        });
                    }
                }

                type_term(self, expr, lvl)
            }

            Expr::List { exprs, span: _ } => Ok({
                let expr = exprs
                    .iter()
                    .flat_map(|ast| type_term(self, ast, lvl))
                    .collect_vec();

                if let Some(fst) = expr.first() {
                    let homo = expr.iter().all(|r| r == fst);
                    if homo {
                        return Ok(Ty::List(vec![fst.clone()]));
                    }
                }

                Ty::List(expr)
            }),

            // Primitives
            Expr::NixString(_) => Ok(String),
            Ast::NixPath(_) => Ok(Path),
            Ast::Null(_) => Ok(Null),
            Ast::Bool { .. } => Ok(Bool),
            Ast::Int { .. } | Ast::Float { .. } => Ok(Number),
            Ast::Comment(_) | Ast::DocComment(_) | Ast::LineComment(_) => unimplemented!(),
        }
    }

    fn load_inherit(
        &mut self,
        lvl: usize,
        inherit: &[Inherit],
    ) -> Result<Vec<(String, ContextType)>, SpannedError> {
        let (ok, err): (Vec<_>, Vec<_>) = inherit
            .iter()
            .map(|Inherit { name, items }| {
                if let Some(expr) = name {
                    let ty = type_term(ctx, expr, lvl)?;

                    match &ty {
                        ty @ Ty::Var(_) => {
                            let vars = items
                                .iter()
                                .map(|(_span, name)| {
                                    (name.to_string(), Ty::Var(ctx.fresh_var(lvl)))
                                })
                                .collect_vec();
                            let record = Ty::Record(vars.clone().into_iter().collect());

                            constrain(ctx, ty, &record).map_err(|e| e.span(expr.get_span()))?;

                            Ok(vars
                                .into_iter()
                                .map(|(name, ty)| (name, ContextType::Type(ty)))
                                .collect())
                        }
                        Ty::Record(rc_items) => {
                            let (ok, err): (Vec<_>, Vec<_>) = items
                                .iter()
                                .map(|(range, name)| {
                                    Ok((
                                        name.to_string(),
                                        ContextType::Type(
                                            rc_items
                                                .get(name)
                                                .ok_or(
                                                    InferError::MissingRecordField {
                                                        field: name.clone(),
                                                    }
                                                    .span(range),
                                                )?
                                                .clone(),
                                        ),
                                    ))
                                })
                                .partition_map(|r| match r {
                                    Ok(ty) => Either::Left(ty),
                                    Err(e) => Either::Right(e),
                                });
                            if err.is_empty() {
                                Ok(ok)
                            } else {
                                Err(SpannedError {
                                    error: InferError::MultipleErrors(err),
                                    span: span.clone(),
                                })
                            }
                        }
                        _ => Err(SpannedError {
                            error: InferError::TypeMismatch {
                                expected: TypeName::Record,
                                found: ty.get_name(),
                            },
                            span: span.clone(),
                        }),
                    }
                } else {
                    let (ok, err): (Vec<_>, Vec<_>) = items
                        .iter()
                        .map(|(range, name)| {
                            Ok((
                                name.to_string(),
                                ctx.lookup(name)
                                    .ok_or(InferError::UnknownIdentifier.span(range))?
                                    .clone(),
                            ))
                        })
                        .partition_map(|r| match r {
                            Ok(v) => Either::Left(v),
                            Err(v) => Either::Right(v),
                        });

                    if err.is_empty() {
                        Ok(ok)
                    } else {
                        Err(SpannedError {
                            error: InferError::MultipleErrors(err),
                            span: span.clone(),
                        })
                    }
                }
            })
            .partition_map(|r| match r {
                Ok(v) => Either::Left(v),
                Err(v) => Either::Right(v),
            });

        if !err.is_empty() {
            Err(SpannedError {
                error: InferError::MultipleErrors(err),
                span: span.clone(),
            })
        } else {
            Ok(ok.into_iter().flatten().collect())
        }
    }

    fn finish(mut self) -> InferenceResult {
        let mut i = Collector::new(&mut self.table);

        let name_cnt = self.module.names().len();
        let expr_cnt = self.module.exprs().len();
        let mut name_ty_map = ArenaMap::with_capacity(name_cnt);
        let mut expr_ty_map = ArenaMap::with_capacity(expr_cnt);
        for (name, _) in self.module.names() {
            let ty = TyVar(u32::from(name.into_raw()));
            name_ty_map.insert(name, i.collect(ty));
        }
        for (expr, _) in self.module.exprs() {
            let ty = TyVar(name_cnt as u32 + u32::from(expr.into_raw()));
            expr_ty_map.insert(expr, i.collect(ty));
        }

        InferenceResult {
            name_ty_map,
            expr_ty_map,
        }
    }
}

/// Traverse the table and freeze all `Ty`s into immutable ones.
struct Collector<'a> {
    cache: Vec<Option<super::Ty>>,
    table: &'a mut UnionFind<Ty>,
}

impl<'a> Collector<'a> {
    fn new(table: &'a mut UnionFind<Ty>) -> Self {
        Self {
            cache: vec![None; table.len()],
            table,
        }
    }

    fn collect(&mut self, ty: TyVar) -> super::Ty {
        let i = self.table.find(ty.0);
        if let Some(ty) = self.cache[i as usize].clone() {
            return ty;
        }

        // Prevent cycles.
        self.cache[i as usize] = Some(super::Ty::Unknown);
        let ret = self.collect_uncached(i);
        self.cache[i as usize] = Some(ret.clone());
        ret
    }

    fn collect_uncached(&mut self, i: u32) -> super::Ty {
        let ty = mem::replace(self.table.get_mut(i), Ty::Unknown);
        match ty {
            Ty::Unknown => super::Ty::Unknown,
            Ty::Bool => super::Ty::Bool,
            Ty::Int => super::Ty::Int,
            Ty::Float => super::Ty::Float,
            Ty::String => super::Ty::String,
            Ty::Path => super::Ty::Path,
            Ty::List(a) => super::Ty::List(self.collect(a).into()),
            Ty::Lambda(a, b) => {
                let a = self.collect(a);
                let b = self.collect(b);
                super::Ty::Lambda(a.into(), b.into())
            }
            Ty::Attrset(fields) => {
                let fields = fields
                    .fields
                    .into_iter()
                    .map(|(name, (ty, src))| (name, self.collect(ty), src))
                    .collect();
                super::Ty::Attrset(super::Attrset { fields, rest: None })
            }
            Ty::External(ty) => ty,
        }
    }
}
