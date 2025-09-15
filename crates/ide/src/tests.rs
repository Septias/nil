use crate::{
    Change, DefDatabase, FileId, FilePos, FileRange, FileSet, FlakeGraph, FlakeInfo,
    SourceDatabase, SourceRoot, SourceRootId, TyDatabase, VfsPath,
};
use anyhow::{bail, ensure, Context, Result};
use indexmap::IndexMap;
use nix_interop::{DEFAULT_IMPORT_FILE, FLAKE_FILE};
use std::collections::HashMap;
use std::sync::Arc;
use std::{mem, ops};
use syntax::ast::AstNode;
use syntax::{NixLanguage, SyntaxNode, TextRange, TextSize};

pub const MARKER_INDICATOR: char = '$';

#[salsa::db]
#[derive(Default, Clone)]
pub struct TestDB {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl salsa::Database for TestDB {}

impl SourceDatabase for TestDB {
    fn file_content(&self, file_id: FileId) -> Arc<str> {
        todo!()
    }

    fn set_file_content(&self, file_id: FileId, content: &str) {
        todo!()
    }

    fn source_root(&self, sid: SourceRootId) -> Arc<SourceRoot> {
        todo!()
    }

    fn set_source_root(&self, sid: SourceRootId, source_root: SourceRoot) {
        todo!()
    }

    fn source_root_flake_info(&self, sid: SourceRootId) -> Option<Arc<FlakeInfo>> {
        todo!()
    }

    fn set_source_root_flake_info(&self, sid: SourceRootId, flake_info: FlakeInfo) {
        todo!()
    }

    fn file_source_root(&self, file_id: FileId) -> SourceRootId {
        todo!()
    }

    fn set_file_source_root(&self, file: FileId, source_id: SourceRootId) {
        todo!()
    }

    fn flake_graph(&self) -> Arc<FlakeGraph> {
        todo!()
    }

    fn set_flake_graph(&self, flake_grap: FlakeGraph) {
        todo!()
    }

    fn nixos_options(&self) -> Arc<NixosOptions> {
        todo!()
    }

    fn set_nixos_options(&self, nixos_options: NixosOptions) {
        todo!()
    }

    #[doc(hidden)]
    fn zalsa_register_downcaster(&self) {
        todo!()
    }

    #[doc = " Downcast a [`dyn Database`] to a [`dyn SourceDatabase`]"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r""]
    #[doc = r" The input database must be of type `Self`."]
    #[doc(hidden)]
    unsafe fn downcast(db: &dyn salsa::plumbing::Database) -> &dyn SourceDatabase
    where
        Self: Sized,
    {
        todo!()
    }
}

impl DefDatabase for TestDB {
    fn intern_path(&self, path_data: PathData) -> Path {
        todo!()
    }

    fn module_with_source_map(&self, file_id: FileId) -> (Arc<Module>, Arc<ModuleSourceMap>) {
        todo!()
    }

    fn module(&self, file_id: FileId) -> Arc<Module> {
        todo!()
    }

    fn source_map(&self, file_id: FileId) -> Arc<ModuleSourceMap> {
        todo!()
    }

    fn module_kind(&self, file_id: FileId) -> Arc<ModuleKind> {
        todo!()
    }

    fn module_references(&self, file_id: FileId) -> Arc<HashSet<FileId>> {
        todo!()
    }

    fn source_root_referrer_graph(
        &self,
        sid: SourceRootId,
    ) -> Arc<HashMap<FileId, ModuleReferrers>> {
        todo!()
    }

    fn source_root_closure(&self, id: SourceRootId) -> Arc<HashSet<FileId>> {
        todo!()
    }

    fn module_referrers(&self, file_id: FileId) -> ModuleReferrers {
        todo!()
    }

    fn resolve_path(&self, path: Path) -> Option<VfsPath> {
        todo!()
    }

    fn scopes(&self, file_id: FileId) -> Arc<ModuleScopes> {
        todo!()
    }

    fn name_resolution(&self, file_id: FileId) -> Arc<NameResolution> {
        todo!()
    }

    fn name_reference(&self, file_id: FileId) -> Arc<NameReference> {
        todo!()
    }

    fn liveness_check(&self, file_id: FileId) -> Arc<LivenessCheckResult> {
        todo!()
    }

    #[doc(hidden)]
    fn zalsa_register_downcaster(&self) {
        todo!()
    }

    #[doc = " Downcast a [`dyn Database`] to a [`dyn DefDatabase`]"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r""]
    #[doc = r" The input database must be of type `Self`."]
    #[doc(hidden)]
    unsafe fn downcast(db: &dyn salsa::plumbing::Database) -> &dyn DefDatabase
    where
        Self: Sized,
    {
        todo!()
    }
}

impl TyDatabase for TestDB {
    fn module_expected_ty(&self, file: FileId) -> Option<Ty> {
        todo!()
    }

    fn infer(&self, file: FileId) -> Arc<InferenceResult> {
        todo!()
    }

    fn nixos_config_ty(&self) -> Ty {
        todo!()
    }

    fn flake_input_tys(&self, sid: SourceRootId) -> Arc<HashMap<String, Ty>> {
        todo!()
    }

    #[doc(hidden)]
    fn zalsa_register_downcaster(&self) {
        todo!()
    }

    #[doc = " Downcast a [`dyn Database`] to a [`dyn TyDatabase`]"]
    #[doc = r""]
    #[doc = r" # Safety"]
    #[doc = r""]
    #[doc = r" The input database must be of type `Self`."]
    #[doc(hidden)]
    unsafe fn downcast(db: &dyn salsa::plumbing::Database) -> &dyn TyDatabase
    where
        Self: Sized,
    {
        todo!()
    }
}

impl TestDB {
    pub fn single_file(fixture: &str) -> Result<(Self, FileId)> {
        let (db, f) = Self::from_fixture(fixture)?;
        ensure!(f.files().len() == 1, "Fixture contains multiple files");
        let file_id = f.files()[0];
        Ok((db, file_id))
    }

    pub fn from_fixture(fixture: &str) -> Result<(Self, Fixture)> {
        let f = Fixture::new(fixture)?;
        let mut db = Self::default();
        let mut change = Change::default();
        let mut file_set = FileSet::default();
        for (i, (path, text)) in (0u32..).zip(&f.files) {
            let file = FileId(i);
            file_set.insert(file, path.clone());
            change.change_file(file, text.to_owned().into());
        }
        let entry = file_set.file_for_path(&VfsPath::new(format!("/{DEFAULT_IMPORT_FILE}")));
        change.set_roots(vec![SourceRoot::new_local(file_set, entry)]);
        let flake_graph = FlakeGraph {
            nodes: HashMap::from_iter(f.flake_info.clone().map(|info| (SourceRootId(0), info))),
        };
        change.set_flake_graph(flake_graph);
        db.set_nixos_options(Arc::default());
        change.apply(&mut db);
        Ok((db, f))
    }

    pub fn find_node<T>(&self, fpos: FilePos, f: impl FnMut(SyntaxNode) -> Option<T>) -> Option<T> {
        self.parse(fpos.file_id)
            .syntax_node()
            .token_at_offset(fpos.pos)
            .right_biased()?
            .parent_ancestors()
            .find_map(f)
    }

    pub fn node_at<N: AstNode<Language = NixLanguage>>(&self, fpos: FilePos) -> Option<N> {
        self.find_node(fpos, N::cast)
    }
}

#[derive(Default, Debug)]
pub struct Fixture {
    files: IndexMap<VfsPath, String>,
    file_ids: Vec<FileId>,
    markers: Vec<FilePos>,
    flake_info: Option<FlakeInfo>,
}

impl ops::Index<usize> for Fixture {
    type Output = FilePos;
    fn index(&self, index: usize) -> &Self::Output {
        &self.markers[index]
    }
}

impl<'a> ops::Index<&'a str> for Fixture {
    type Output = FileId;
    fn index(&self, index: &'a str) -> &Self::Output {
        let id = self.files.get_index_of(&VfsPath::new(index)).unwrap();
        &self.file_ids[id]
    }
}

impl Fixture {
    pub fn new(fixture: &str) -> Result<Self> {
        ensure!(
            u32::try_from(fixture.len()).is_ok(),
            "Size too large: {}",
            fixture.len()
        );

        let mut this = Self::default();
        let mut missing_header = false;
        let mut cur_path = None;
        let mut cur_text = String::new();
        let mut cur_file = FileId(0);
        let mut markers = [None; 10];
        for line in fixture.lines().skip_while(|line| line.is_empty()) {
            if let Some(header) = line.strip_prefix("#- ") {
                ensure!(!missing_header, "Missing path header at the first line");

                let mut iter = header.split(' ');
                let path = iter.next().context("Missing path")?;
                let is_flake_nix = path == format!("/{FLAKE_FILE}");
                let path = VfsPath::new(path);

                if is_flake_nix {
                    let flake_info = this.flake_info.insert(FlakeInfo {
                        flake_file: cur_file,
                        input_store_paths: HashMap::new(),
                        input_flake_outputs: HashMap::new(),
                    });
                    for prop in iter {
                        if let Some((name, target)) = prop
                            .strip_prefix("input:")
                            .and_then(|input| input.split_once('='))
                        {
                            let target = VfsPath::new(target);
                            flake_info.input_store_paths.insert(name.into(), target);
                        } else {
                            bail!("Unknown property {prop}");
                        }
                    }
                } else if iter.next().is_some() {
                    bail!("Invalid property: {line}");
                }

                if let Some(prev_path) = cur_path.replace(path) {
                    this.insert_file(prev_path, mem::take(&mut cur_text))?;
                    cur_file.0 += 1;
                }
            } else {
                if cur_path.is_none() {
                    missing_header = true;
                    cur_path = Some(VfsPath::new(format!("/{DEFAULT_IMPORT_FILE}")));
                }

                let mut iter = line.chars().peekable();
                while let Some(ch) = iter.next() {
                    if ch == MARKER_INDICATOR
                        && matches!(iter.peek(), Some(c) if c.is_ascii_digit())
                    {
                        let n = iter.next().unwrap().to_digit(10).unwrap() as usize;
                        let pos =
                            FilePos::new(cur_file, TextSize::try_from(cur_text.len()).unwrap());
                        ensure!(
                            markers[n].replace(pos).is_none(),
                            "Duplicated marker: {}",
                            n
                        );
                    } else {
                        cur_text.push(ch);
                    }
                }
                cur_text += "\n";
            }
        }
        this.insert_file(cur_path.context("Empty fixture")?, cur_text)?;

        let marker_len = markers
            .iter()
            .rposition(|p| p.is_some())
            .map_or(0, |n| n + 1);
        this.markers = markers
            .into_iter()
            .take(marker_len)
            .enumerate()
            .map(|(i, p)| p.with_context(|| format!("Discontinuous marker: {i}")))
            .collect::<Result<Vec<_>>>()?;

        Ok(this)
    }

    fn insert_file(&mut self, path: VfsPath, mut text: String) -> Result<()> {
        let file = FileId(self.files.len() as u32);
        // Trim the last newline for simplicity. But whitespaces are kept since markers may be
        // pointing to them.
        text.truncate(text.trim_end_matches('\n').len());
        ensure!(
            self.files.insert(path.clone(), text).is_none(),
            "Duplicated path: {:?}",
            path
        );
        self.file_ids.push(file);
        Ok(())
    }

    pub fn files(&self) -> &[FileId] {
        &self.file_ids
    }

    pub fn markers(&self) -> &[FilePos] {
        &self.markers
    }

    #[track_caller]
    pub fn unwrap_single_range_marker(&self) -> FileRange {
        match *self.markers() {
            [fpos] => FileRange::empty(fpos),
            [start, end] => {
                assert_eq!(
                    start.file_id, end.file_id,
                    "Start and end markers must be in the same file"
                );
                FileRange::new(start.file_id, TextRange::new(start.pos, end.pos))
            }
            _ => panic!("Must have either 1 or 2 markers"),
        }
    }
}
