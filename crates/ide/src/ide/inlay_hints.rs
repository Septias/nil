use crate::def::AstPtr;
use crate::ty::Ty;
use crate::{FileId, InferenceResult, ModuleSourceMap, TyDatabase};
use std::sync::Arc;
use syntax::ast::{self, AstNode};
use syntax::rowan::WalkEvent;
use syntax::{SyntaxNode, TextSize};

#[derive(Debug)]
pub struct InlayHintResult {
    pub position: TextSize,
    pub ty: Ty,
}

pub(crate) fn inlay_hints(db: &dyn TyDatabase, file_id: FileId) -> Vec<InlayHintResult> {
    let parse = db.parse(file_id);
    parse.syntax_node().kind();
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
    fn push_symbol(&mut self, ty: Ty, position: TextSize) {
        self.diagnostics.push(InlayHintResult { position, ty })
    }

    fn collect_node(&mut self, n: &SyntaxNode) {
        let mut iter = n.preorder();
        while let Some(event) = iter.next() {
            let n = match event {
                WalkEvent::Enter(n) => n,
                WalkEvent::Leave(_) => {
                    continue;
                }
            };
            let Some(binding) = ast::Attr::cast(n) else {
                continue;
            };
            let name = self
                .source_map
                .name_for_node(AstPtr::new(&binding.syntax()));

            if let Some(name) = name {
                let ty = self.types.ty_for_name(name);
                let pos = binding.syntax().text_range().end();
                self.push_symbol(ty, pos)
            }
            // match binding {
            //     ast::Attr::Dynamic(dynamic) => todo!(),
            //     ast::Attr::Name(_name) => {
            //     }
            //     ast::Attr::String(_) => todo!(),
            // }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::TestDB;

    #[test]
    fn inlay_simple() {
        let (db, file) = TestDB::single_file("let a = 2 in {}").unwrap();
        let syms = inlay_hints(&db, file);
        println!("{syms:?}")
    }
}
