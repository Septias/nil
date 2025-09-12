

# Components

## Salsa
> Incremental Recompilation.


## LA Arena
> Fast arena for bulk allocation and deletion.


## VFS
> Virtual file system to handle files from `SourceRoot`s these are used to be operating system agnostic.
- Creates line maps
- Translates between FilePos and (col, char)

## Module trees
- This is needed such that syntax trees are relatively change agnostic and we can still retrieve the syntax nodes back

## Async LSP
> Asynchronous LSP.



# Structure
- LSP
  - `FileId`: used to reference a single file.
  - `ExprId`: used to reference a single expression. This is needed such that syntax trees are relatively change agnostic and we can still retrieve the syntax nodes back.

- `AnalysisHost`: Host multiple `Analyses`. Clone them and create cancelable requests.
- `Analyses`    : A single analysis which might be a snapshot for a request. Immutable, changes are requested with a `Change`
- `Change`      : A change to analysis. This one is applied to the `salsa` database.
