use crate::{FileId, FilePos, TyDatabase};

pub(crate) struct InlayHint {
    position: FilePos,
    label: String,
}
pub(crate) fn inlay_hints(db: &dyn TyDatabase, file: FileId) -> Option<Vec<InlayHint>> {
    let _infer = db.infer(file);
    todo!();
}
