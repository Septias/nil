use syntax::TextRange;

use crate::{FileId, FilePos, TyDatabase};

pub struct InlayHintResult {
    position: FilePos,
    label: String,
}

pub(crate) fn inlay_hints(
    db: &dyn TyDatabase,
    file: FileId,
    range: TextRange,
) -> Option<Vec<InlayHintResult>> {
    let _infer = db.infer(file);
    todo!();
}
