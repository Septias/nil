use crate::ide::RootDatabase;
use nix_interop::flake_output::FlakeOutput;
use nix_interop::nixos_options::{NixosOption, NixosOptions};
use salsa::{Durability, Setter};
use std::collections::HashMap;
use std::fmt;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use syntax::{TextRange, TextSize};

use crate::ide::RootDatabase;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FileId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SourceRootId(pub u32);

#[derive(Debug)]
#[salsa::input]
pub(crate) struct File {
    #[returns(ref)]
    content: String,
}

/// An path in the virtual filesystem.
#[cfg(unix)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VfsPath {
    Path(PathBuf),
    Virtual(String),
}

impl VfsPath {
    /// Construct a new filesystem path.
    #[must_use]
    pub fn new(path: impl AsRef<Path>) -> Self {
        Self::Path(path.as_ref().to_path_buf())
    }

    /// Return a reference to the underlying `Path`.
    #[must_use]
    pub fn as_path(&self) -> Option<&Path> {
        match self {
            Self::Path(path) => Some(path),
            Self::Virtual(_) => None,
        }
    }

    /// Extends `self` with `path`.
    ///
    /// Returns `None` if the path is not extentable, otherwise returns `Some(())`.
    #[must_use]
    pub fn push(&mut self, path: &str) -> Option<()> {
        match self {
            Self::Path(this) => {
                this.push(path);
                Some(())
            }
            Self::Virtual(_) => None,
        }
    }

    /// Creates a new `VfsPath` with `path` adjoined to self.
    #[must_use]
    pub fn join(&self, path: &str) -> Option<Self> {
        match self {
            Self::Path(this) => Some(Self::Path(this.join(path))),
            Self::Virtual(_) => None,
        }
    }

    /// Truncates `self` to the parent of it.
    ///
    /// Returns `false` and does nothing if `self` has no parent,
    /// otherwise, return `true`.
    pub fn pop(&mut self) -> bool {
        match self {
            Self::Path(this) => this.pop(),
            Self::Virtual(_) => false,
        }
    }

    /// Returns an `impl Display` struct for human.
    #[must_use]
    pub fn display(&self) -> impl fmt::Display + '_ {
        struct Display<'a>(&'a VfsPath);

        impl fmt::Display for Display<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self.0 {
                    VfsPath::Path(path) => path.display().fmt(f),
                    VfsPath::Virtual(id) => write!(f, "(virtual path {id})"),
                }
            }
        }

        Display(self)
    }
}

impl From<PathBuf> for VfsPath {
    fn from(path: PathBuf) -> Self {
        Self::Path(path)
    }
}

impl From<&'_ Path> for VfsPath {
    fn from(path: &'_ Path) -> Self {
        Self::Path(path.to_path_buf())
    }
}

/// A set of [`VfsPath`]s identified by [`File`]s.
#[derive(Default, Clone, PartialEq, Eq)]
pub struct FileSet {
    files: HashMap<VfsPath, File>,
    paths: HashMap<File, VfsPath>,
}

impl FileSet {
    pub fn insert(&mut self, file: File, path: VfsPath) {
        self.files.insert(path.clone(), file);
        self.paths.insert(file, path);
    }

    pub fn remove_file(&mut self, file: File) {
        if let Some(path) = self.paths.remove(&file) {
            self.files.remove(&path);
        }
    }

    pub fn file_for_path(&self, path: &VfsPath) -> Option<File> {
        self.files.get(path).copied()
    }

    pub fn path_for_file(&self, file: File) -> &VfsPath {
        &self.paths[&file]
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = (File, &'_ VfsPath)> + '_ {
        self.paths.iter().map(|(&file, path)| (file, path))
    }
}

impl fmt::Debug for FileSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(&self.paths).finish()
    }
}

/// A workspace unit, typically a flake package.
/// It is an abstraction about files in a coding project. Files are stored in virtual file system [Vfs]
/// and the source root handles relative and os agnostic paths.
pub struct SourceRoot {
    file_set: FileSet,
    entry: Option<File>,
}

impl SourceRoot {
    pub fn new_local(file_set: FileSet, entry: Option<File>) -> Self {
        Self {
            file_set,
            entry: entry,
        }
    }

    pub fn file_for_path(&self, path: &VfsPath) -> Option<File> {
        self.file_set.file_for_path(path)
    }

    pub fn path_for_file(&self, file: File) -> &VfsPath {
        self.file_set.path_for_file(file)
    }

    pub fn files(&self) -> impl ExactSizeIterator<Item = (File, &'_ VfsPath)> + '_ {
        self.file_set.iter()
    }

    pub fn entry(&self) -> Option<File> {
        self.entry
    }
}

#[salsa::input(debug)]
pub struct FlakeGraph {
    pub nodes: HashMap<SourceRootId, FlakeInfo>,
}

// FIXME: Make this a tree structure.
#[salsa::input]
pub struct FlakeInfo {
    pub flake_file: File,
    pub input_store_paths: HashMap<String, VfsPath>,
    pub input_flake_outputs: HashMap<String, FlakeOutput>,
}

impl fmt::Debug for FlakeInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FlakeInfo")
            .field("flake_file", &self.flake_file)
            .field("input_store_paths", &self.input_store_paths)
            .field("input_flake_outputs", &self.input_flake_outputs.keys())
            .finish_non_exhaustive()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct InFile<T> {
    pub file_id: File,
    pub value: T,
}

impl<T> InFile<T> {
    pub fn new(file_id: File, value: T) -> Self {
        Self { file_id, value }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> InFile<U> {
        InFile {
            file_id: self.file_id,
            value: f(self.value),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct FilePos {
    pub file_id: File,
    pub pos: TextSize,
}

impl FilePos {
    pub fn new(file_id: File, pos: TextSize) -> Self {
        Self { file_id, pos }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct FileRange {
    pub file_id: File,
    pub range: TextRange,
}

impl FileRange {
    pub fn new(file_id: File, range: TextRange) -> Self {
        Self { file_id, range }
    }

    pub fn empty(pos: FilePos) -> Self {
        Self::new(pos.file_id, TextRange::empty(pos.pos))
    }
}

#[salsa::db]
pub trait SourceDatabase: salsa::Database {
    // The source root
    fn source_root(&self, sid: SourceRootId) -> Arc<SourceRoot>;
    fn set_source_root(&self, sid: SourceRootId, source_root: SourceRoot);

    fn source_root_flake_info(&self, sid: SourceRootId) -> Option<Arc<FlakeInfo>>;
    fn set_source_root_flake_info(&self, sid: SourceRootId, flake_info: FlakeInfo);

    fn file_source_root(&self, file_id: File) -> SourceRootId;
    fn set_file_source_root(&self, file: File, source_id: SourceRootId);

    fn flake_graph(&self) -> Arc<FlakeGraph>;
    fn set_flake_graph(&self, flake_grap: FlakeGraph);

    fn nixos_options(&self) -> Arc<NixosOptions>;
    fn set_nixos_options(&self, nixos_options: NixosOptions);
}

#[allow(unused)]
#[salsa::db]
impl SourceDatabase for RootDatabase {
    fn file_content(&self, file_id: File) -> Arc<str> {
        todo!()
    }
    fn set_file_content(&self, file_id: File, content: &str) {

        //  Durability::LOW
    }
    fn source_root(&self, sid: SourceRootId) -> Arc<SourceRoot> {
        todo!()
    }

    fn source_root_flake_info(&self, sid: SourceRootId) -> Option<Arc<FlakeInfo>> {
        todo!()
    }

    fn file_source_root(&self, file_id: File) -> SourceRootId {
        todo!()
    }

    fn flake_graph(&self) -> Arc<FlakeGraph> {
        todo!()
    }

    fn nixos_options(&self) -> Arc<NixosOptions> {
        todo!()
    }

    fn set_source_root(&self, sid: SourceRootId, source_root: SourceRoot) {
        // Durability::HIGH
        todo!()
    }

    fn set_source_root_flake_info(&self, sid: SourceRootId, flake_info: FlakeInfo) {
        todo!()
    }

    fn set_file_source_root(&self, file_id: File, source_id: SourceRootId) {
        todo!()
    }

    fn set_flake_graph(&self, flake_grap: FlakeGraph) {
        todo!()
    }

    fn set_nixos_options(&self, nixos_options: NixosOptions) {
        // durability:Medium
        todo!()
    }
}

fn source_root_flake_info(db: &dyn SourceDatabase, sid: SourceRootId) -> Option<Arc<FlakeInfo>> {
    db.flake_graph().nodes(db).get(&sid).cloned().map(Arc::new)
}

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Change {
    pub roots: Option<Vec<SourceRoot>>,
    pub file_changes: Vec<(File, Arc<str>)>,
    pub flake_graph: Option<FlakeGraph>,
    pub nixos_options: Option<NixosOptions>,
}

impl Change {
    pub fn is_empty(&self) -> bool {
        self.roots.is_none() && self.file_changes.is_empty()
    }

    pub fn set_flake_graph(&mut self, graph: FlakeGraph) {
        self.flake_graph = Some(graph);
    }

    pub fn set_nixos_options(&mut self, opts: NixosOptions) {
        self.nixos_options = Some(opts);
    }

    pub fn set_roots(&mut self, roots: Vec<SourceRoot>) {
        self.roots = Some(roots);
    }

    pub fn change_file(&mut self, file_id: File, content: Arc<str>) {
        self.file_changes.push((file_id, content));
    }

    pub(crate) fn apply(self, db: &dyn SourceDatabase) {
        if let Some(flake_graph) = self.flake_graph {
            // db.flake_graph()
            //     .set_nodes(db)
            //     .with_durability(Durability::MEDIUM)
            //     .to(flake_graph.nodes(db));
        }
        if let Some(opts) = self.nixos_options {
            db.set_nixos_options(opts);
        }
        if let Some(roots) = self.roots {
            u32::try_from(roots.len()).expect("Length overflow");
            for (sid, root) in (0u32..).map(SourceRootId).zip(roots) {
                for (fid, _) in root.files() {
                    db.set_file_source_root(fid, sid);
                }
                db.set_source_root(sid, root);
            }
        }
        for (file_id, content) in self.file_changes {
            db.set_file_content(file_id, &content);
        }
    }
}

impl fmt::Debug for Change {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let modified = self
            .file_changes
            .iter()
            .filter(|(_, content)| !content.is_empty())
            .count();
        let cleared = self.file_changes.len() - modified;
        f.debug_struct("Change")
            .field("roots", &self.roots.as_ref().map(|roots| roots.len()))
            .field("modified", &modified)
            .field("cleared", &cleared)
            .finish_non_exhaustive()
    }
}
