#[derive(Default)]
pub(crate) struct Chain<A, B> {
    pub first: A,
    pub second: B,
}

/* NOTE: The trailing comma is mandatory for this macro.
 * Also note that a kind of expression is path, so it is important to
 * distinguish them in the pattern.
 */
macro_rules! chain {
    (<$head:path>, $($tail:tt)*) => {
        crate::utils::chain!((<$head>::default()), $($tail)*)
    };
    ($single:expr,) => {
        $single
    };
    ($head:expr, $($tail:tt)+) => {
        crate::utils::Chain {
            first: $head,
            second: crate::utils::chain!($($tail)+)
        }
    };
}

pub(crate) use chain;

pub(crate) mod mir {
    use rustc_hir::definitions::DisambiguatedDefPathData;
    use rustc_middle::ty::TyCtxt;
    use rustc_span::def_id::DefId;

    pub(crate) trait TyCtxtExt<'tcx> {
        fn is_llvm_intrinsic(self, def_id: DefId) -> bool;
        fn module_of(self, def_id: DefId) -> impl Iterator<Item = DisambiguatedDefPathData>;
    }

    impl<'tcx> TyCtxtExt<'tcx> for TyCtxt<'tcx> {
        fn is_llvm_intrinsic(self, def_id: DefId) -> bool {
            // Grabbed from `is_call_from_compiler_builtins_to_upstream_monomorphization`#5aea140.
            if let Some(name) = self.codegen_fn_attrs(def_id).link_name {
                name.as_str().starts_with("llvm.")
            } else {
                false
            }
        }

        fn module_of(self, def_id: DefId) -> impl Iterator<Item = DisambiguatedDefPathData> {
            use rustc_hir::definitions::DefPathData;
            self.def_path(def_id)
                .data
                .into_iter()
                .take_while(|p| matches!(p.data, DefPathData::TypeNs(_)))
        }
    }
}

pub(super) mod file {
    use std::{
        env,
        path::{Path, PathBuf},
    };

    use common::log_debug;

    // The deps folder when building the project. Set by the build script.
    const DIR_DEPS: &str = env!("DEPS_DIR");

    pub(crate) fn find_dependency_path<'a>(
        name: &'static str,
        priority_dirs: impl IntoIterator<Item = &'a Path>,
    ) -> PathBuf {
        try_find_dependency_path(name, priority_dirs)
            .unwrap_or_else(|| panic!("Unable to find the dependency with name: {}", name))
    }

    // Tries to find a dependency of the compiler by searching in the given directories.
    // Order of search is:
    // 1. Priority directories
    // 2. Current working directory
    // 3. Directory of the executable
    pub(crate) fn try_find_dependency_path<'a>(
        name: impl AsRef<Path> + Clone,
        priority_dirs: impl IntoIterator<Item = &'a Path>,
    ) -> Option<PathBuf> {
        let try_dir = |path: &Path| {
            log_debug!(
                "Trying dir in search of `{}`: {:?}",
                name.as_ref().display(),
                path
            );
            common::utils::try_join_path(path, name.clone())
        };

        let try_priority_dirs = || priority_dirs.into_iter().find_map(try_dir);
        let try_deps = || try_dir(DIR_DEPS.as_ref());
        let try_cwd = || env::current_dir().ok().and_then(|p| try_dir(&p));
        let try_exe_path = || {
            env::current_exe()
                .ok()
                .and_then(|p| p.ancestors().skip(1).find_map(try_dir))
        };

        None.or_else(try_priority_dirs)
            .or_else(try_deps)
            .or_else(try_cwd)
            .or_else(try_exe_path)
    }
}
