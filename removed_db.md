```rust
/// Database for types.
#[salsa::query_group(TyDatabaseStorage)]
pub trait TyDatabase: DefDatabase {
    #[salsa::invoke(module_expected_ty)]
    fn module_expected_ty(&self, file: FileId) -> Option<Ty>;

    #[salsa::invoke(infer::infer_query)]
    fn infer(&self, file: FileId) -> Arc<InferenceResult>;

    #[salsa::invoke(convert::options_to_config_ty)]
    fn nixos_config_ty(&self) -> Ty;

    #[salsa::invoke(convert::flake_input_tys)]
    fn flake_input_tys(&self, sid: SourceRootId) -> Arc<HashMap<String, Ty>>;
}
```
