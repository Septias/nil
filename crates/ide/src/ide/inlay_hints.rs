use std::sync::Arc;

use crate::def::{AstPtr, NameId};
use crate::ty::Ty;
use crate::{DefDatabase, FileId, FilePos, InferenceResult, ModuleSourceMap, TyDatabase};
use syntax::ast::{self, AstNode};
use syntax::rowan::WalkEvent;
use syntax::{SyntaxNode, TextRange};

#[derive(Debug)]
pub struct InlayHintResult {
    position: FilePos,
    ty: Ty,
}

pub(crate) fn inlay_hints(db: &dyn TyDatabase, file_id: FileId) -> Vec<InlayHintResult> {
    let parse = db.parse(file_id);
    parse.syntax_node().kind();
    let nameres = db.name_resolution(file_id);
    let source_map = db.source_map(file_id);
    let mut symbols = Vec::new();
    let infer = db.infer(file_id);
    let mut collector = Collector {
        source_map: &source_map,
        diagnostics: &mut symbols,
        types: infer,
    };
    collector.collect_node(&parse.syntax_node());
    symbols
}

#[derive(Debug)]
struct Collector<'a, 'b> {
    source_map: &'a ModuleSourceMap,
    types: Arc<InferenceResult>,
    diagnostics: &'b mut Vec<InlayHintResult>,
}

impl Collector<'_, '_> {
    fn push_symbol(&mut self, ty: Ty, position: FilePos) {
        self.diagnostics.push(InlayHintResult {
            position: todo!(),
            ty: todo!(),
        })
    }

    fn collect_node(&mut self, n: &SyntaxNode) {
        let mut iter = n.preorder();
        let mut last_is_path_value = false;
        while let Some(event) = iter.next() {
            let n = match event {
                WalkEvent::Enter(n) => n,
                WalkEvent::Leave(n) => {
                    last_is_path_value = ast::AttrpathValue::can_cast(n.kind());
                    continue;
                }
            };
            let Some(binding) = ast::Attr::cast(n) else {
                continue;
            };
            match binding {
                ast::Attr::Dynamic(dynamic) => todo!(),
                ast::Attr::Name(name) => {}
                ast::Attr::String(_) => todo!(),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};
    use std::fmt::Write;

    #[track_caller]
    fn check(fixture: &str, expect: Expect) {
        let (db, file) = TestDB::single_file(fixture).unwrap();
        let syms = inlay_hints(&db, file);
        let mut got = String::new();
        expect.assert_eq(&got);
    }
}
