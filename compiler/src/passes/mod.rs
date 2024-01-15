pub(crate) mod ctfe;
mod instr;
pub(crate) mod logger;
mod null;
mod runtime_adder;
pub(crate) mod tyexp;

use std::any::Any;
use std::collections::HashMap;
use std::ops::DerefMut;
use std::sync::{Arc, Mutex};

use rustc_ast as ast;
use rustc_driver::{self as driver, Compilation};
use rustc_interface::{interface, Queries};
use rustc_middle::{mir, ty as mir_ty};

use paste::paste;

use self::implementation::CompilationPassAdapter;
use crate::utils::Chain;

#[cfg(nctfe)]
pub(crate) use ctfe::{CtfeScanner, NctfeFunctionAdder};
pub(crate) use instr::Instrumentor;
pub(crate) use logger::CompilationPassLogExt;
pub(crate) use null::NullPass;
pub(crate) use runtime_adder::RuntimeAdder;
pub(crate) use tyexp::TypeExporter;

pub(super) type Callbacks = Box<dyn driver::Callbacks + Send>;
pub(super) type PrerequisitePass = Chain<RuntimeAdder, NoOpPass>;

pub(crate) trait HasResult<R> {
    fn into_result(self) -> R;
}

macro_rules! visit_before_after {
    (fn $name:ident $($sig:tt)*) => {
        paste!{
            fn [<visit_ $name _before>] $($sig)*

            fn [<visit_ $name _after>] $($sig)*
        }
    };
}

#[allow(unused)]
pub(crate) trait CompilationPass {
    visit_before_after! {
        fn ast(&mut self, krate: &ast::Crate) -> Compilation {
            Compilation::Continue
        }
    }

    fn transform_ast(&mut self, krate: &mut ast::Crate) {
        Default::default()
    }

    fn visit_tcx_after_analysis(
        &mut self,
        tcx: mir_ty::TyCtxt,
        storage: &mut dyn Storage,
    ) -> Compilation {
        Compilation::Continue
    }

    visit_before_after! {
        fn tcx_at_codegen(
            &mut self,
            tcx: mir_ty::TyCtxt,
            storage: &mut dyn Storage,
        ) {
            Default::default()
        }
    }

    visit_before_after! {
        fn mir_body<'tcx>(
            tcx: mir_ty::TyCtxt<'tcx>,
            body: &mir::Body<'tcx>,
            storage: &mut dyn Storage,
        ) {
            Default::default()
        }
    }

    /* As we need function pointers in the query structures, this function cannot
     * take self. */
    fn transform_mir_body<'tcx>(
        tcx: mir_ty::TyCtxt<'tcx>,
        body: &mut mir::Body<'tcx>,
        storage: &mut dyn Storage,
    ) {
        Default::default()
    }
}

#[derive(Default)]
pub(crate) struct NoOpPass;
impl CompilationPass for NoOpPass {}

pub(crate) trait CompilationPassExt {
    fn chain<T>(self, next: T) -> Chain<Self, T>
    where
        Self: Sized,
        T: CompilationPass,
    {
        Chain {
            first: self,
            second: next,
        }
    }

    fn to_callbacks(self) -> Callbacks;
}
impl<T: CompilationPass + Send + Sync + 'static> CompilationPassExt for T {
    fn to_callbacks(self) -> Callbacks {
        Box::new(CompilationPassAdapter(Arc::new(Mutex::new(self))))
    }
}

pub(crate) trait Storage: std::fmt::Debug {
    fn get_raw_or_insert_with<'a>(
        &'a mut self,
        key: String,
        default: Box<dyn FnOnce() -> Box<dyn Any> + 'a>,
    ) -> &mut Box<dyn Any>;
}
impl Storage for HashMap<String, Box<dyn Any>> {
    fn get_raw_or_insert_with<'a>(
        &'a mut self,
        key: String,
        default: Box<dyn FnOnce() -> Box<dyn Any> + 'a>,
    ) -> &mut Box<dyn Any> {
        self.entry(key).or_insert_with(default)
    }
}

pub(crate) trait StorageExt {
    fn get_or_insert_with<'a, V: Any>(
        &'a mut self,
        key: String,
        default: impl FnOnce() -> V + 'a,
    ) -> &mut V;

    fn get_or_default<V: Default + Any>(&mut self, key: String) -> &mut V {
        self.get_or_insert_with(key, V::default)
    }
}
impl<T: Storage + ?Sized> StorageExt for T {
    fn get_or_insert_with<'a, V: Any>(
        &'a mut self,
        key: String,
        default: impl FnOnce() -> V + 'a,
    ) -> &mut V {
        self.get_raw_or_insert_with(key, Box::new(|| Box::new(default())))
            .deref_mut()
            .downcast_mut::<V>()
            .unwrap()
    }
}

mod implementation {
    use super::*;

    use mir_ty::TyCtxt;
    use rustc_driver::Compilation;
    use rustc_span::def_id::LocalDefId;

    use std::cell::Cell;

    /* NOTE: How does `queries` work?
     * Here, the queries object is a structure that performs compiler-related
     * operations in a lazy fashion (on the first call they will be performed and
     * the result is reused afterwards).
     * The result is stored in the shape of Steel object. A `Steel` permits
     * immutable and mutable borrows to the data it holds. Also, provides a `steel`
     * mechanism to take the ownership of the data. Once it is done, the data cannot
     * be accessed anymore. The important point is that the driver itself steels the
     * data at particular point. For example, the result of `parse` query is consumed
     * (stolen) by the driver. Therefore, it is not possible to access it in the
     * callbacks following `after_parsing`.
     */

    /* NOTE: What happen inside the driver?
     * In short, it is wrapper around `rustc_interface`. It creates the appropriate
     * configuration for it and then calls compilation functions — mostly on the
     * `queries` object — and provides feedback (using callbacks) at some phases.
     *
     * At the first step it gives us the configuration it has created
     * (based on args) for the interface (`config` callback).
     *
     * Next, parsing happens and `after_parsing` gets called. We can access the AST
     * using the `parse` query.
     * Next, it expands macros and `after_expansion` gets called.
     * Next, it performs some analysis (not sure what at the moment) and
     * `after_analysis` gets called.
     *
     * After this point, the driver starts to perform the actual compilation.
     * It is done through `ongoing_codegen` and `linker` in the final steps.
     *
     * It looks like `override_queries` is only called in `create_global_ctxt` inside
     * `rustc_interface`, which is called for creating `global_ctxt` that is done
     * after the parsing and before the expansion, or as the documentation says
     * "just after we have populated the list of queries".
     */

    /* NOTE: What is the relation between provider functions?
     * It is not tracked completely at the moment. However, the following call trace
     * is identified in the last investigation:
     * optimized_mir
     * -> mir_drops_elaborated_and_const_checked
     * -> mir_promoted
     * -> mir_const (not just for constants)
     * -> mir_built (the actual translation to MIR)
     *
     * Note that most of these functions are query thus memoized, which means that their result
     * will be stored and later calls will give the same instance. Also, it means
     * you have only one chance to perform any manipulation.
     */

    thread_local! {
        /*
         * NOTE: Both `override_queries` and `optimized_mir` are function pointers, which means
         * that they cannot be closures with captured variables. Therefore, we use these statically
         * allocated cells to store the original functions.
         * As there will be a single transformation pass in the project and the original pointer
         * is the same anyway, this should not cause any problems.
         */
        #[allow(clippy::type_complexity)]
        static ORIGINAL_OVERRIDE: Cell<
            Option<fn(&rustc_session::Session, &mut rustc_middle::util::Providers)>,
        > = Cell::new(None);
        static ORIGINAL_OPTIMIZED_MIR: Cell<
            for<'tcx> fn(TyCtxt<'tcx>, LocalDefId) -> &mir::Body<'tcx>
        > = Cell::new(|_, _| unreachable!());
    }

    pub(super) struct CompilationPassAdapter<T: CompilationPass + Send + ?Sized>(pub Arc<Mutex<T>>);

    macro_rules! stop_if_stop {
        ($result:expr) => {
            if $result == Compilation::Stop {
                return Compilation::Stop;
            }
        };
    }

    impl<T> driver::Callbacks for CompilationPassAdapter<T>
    where
        T: CompilationPass + Send + Sync + ?Sized + 'static,
    {
        fn config(&mut self, config: &mut interface::Config) {
            ORIGINAL_OVERRIDE.set(config.override_queries.take());

            config.override_queries = Some(move |session, providers| {
                if let Some(existing_override) = ORIGINAL_OVERRIDE.get() {
                    existing_override(session, providers);
                }

                ORIGINAL_OPTIMIZED_MIR.set(providers.optimized_mir);
                providers.optimized_mir = Self::optimized_mir;
            });

            config.make_codegen_backend = Some(codegen::get_backend_maker(self.0.clone()));

            global::clear_ctxt_id_and_storage();
        }

        fn after_crate_root_parsing<'tcx>(
            &mut self,
            _compiler: &interface::Compiler,
            queries: &'tcx Queries<'tcx>,
        ) -> Compilation {
            let mut ast_steal = queries.parse().unwrap();

            let mut pass = self.0.lock().unwrap();
            stop_if_stop!(pass.visit_ast_before(&ast_steal.borrow()));
            pass.transform_ast(ast_steal.get_mut());
            stop_if_stop!(pass.visit_ast_after(&ast_steal.borrow()));

            Compilation::Continue
        }

        fn after_expansion<'tcx>(
            &mut self,
            _compiler: &interface::Compiler,
            _queries: &'tcx Queries<'tcx>,
        ) -> Compilation {
            Compilation::Continue
        }

        fn after_analysis<'tcx>(
            &mut self,
            _compiler: &interface::Compiler,
            queries: &'tcx Queries<'tcx>,
        ) -> Compilation {
            queries.global_ctxt().unwrap().enter(global::set_ctxt_id);
            queries.global_ctxt().unwrap().enter(|tcx| {
                global::with_storage(tcx, |storage| {
                    let mut pass = self.0.lock().unwrap();
                    pass.visit_tcx_after_analysis(tcx, storage)
                })
            })
        }
    }

    impl<T: CompilationPass + Send + ?Sized> CompilationPassAdapter<T> {
        fn optimized_mir(tcx: TyCtxt, id: LocalDefId) -> &mir::Body {
            /* NOTE: Currently, it seems that there is no way to deallocate
             * something from arena. So, we have to clone the body. */
            let mut body = ORIGINAL_OPTIMIZED_MIR.get()(tcx, id).clone();
            global::with_storage(tcx, |storage| {
                T::visit_mir_body_before(tcx, &body, storage);
                T::transform_mir_body(tcx, &mut body, storage);
                T::visit_mir_body_after(tcx, &body, storage);
            });
            tcx.arena.alloc(body)
        }
    }

    impl<A, B> CompilationPass for Chain<A, B>
    where
        A: CompilationPass,
        B: CompilationPass,
    {
        fn visit_ast_before(&mut self, krate: &ast::Crate) -> Compilation {
            stop_if_stop!(self.first.visit_ast_before(krate));
            stop_if_stop!(self.second.visit_ast_before(krate));

            Compilation::Continue
        }

        fn visit_ast_after(&mut self, krate: &ast::Crate) -> Compilation {
            // NOTE: We perform in the telescope order.
            stop_if_stop!(self.second.visit_ast_after(krate));
            stop_if_stop!(self.first.visit_ast_after(krate));

            Compilation::Continue
        }

        fn visit_tcx_after_analysis(
            &mut self,
            tcx: mir_ty::TyCtxt,
            storage: &mut dyn Storage,
        ) -> Compilation {
            stop_if_stop!(self.first.visit_tcx_after_analysis(tcx, storage));
            stop_if_stop!(self.second.visit_tcx_after_analysis(tcx, storage));

            Compilation::Continue
        }

        fn visit_tcx_at_codegen_before(&mut self, tcx: mir_ty::TyCtxt, storage: &mut dyn Storage) {
            self.first.visit_tcx_at_codegen_before(tcx, storage);
            self.second.visit_tcx_at_codegen_before(tcx, storage);
        }

        fn visit_tcx_at_codegen_after(&mut self, tcx: mir_ty::TyCtxt, storage: &mut dyn Storage) {
            self.first.visit_tcx_at_codegen_after(tcx, storage);
            self.second.visit_tcx_at_codegen_after(tcx, storage);
        }

        fn visit_mir_body_before<'tcx>(
            tcx: mir_ty::TyCtxt<'tcx>,
            body: &mir::Body<'tcx>,
            storage: &mut dyn Storage,
        ) {
            A::visit_mir_body_before(tcx, body, storage);
            B::visit_mir_body_before(tcx, body, storage);
        }

        fn visit_mir_body_after<'tcx>(
            tcx: mir_ty::TyCtxt<'tcx>,
            body: &mir::Body<'tcx>,
            storage: &mut dyn Storage,
        ) {
            A::visit_mir_body_after(tcx, body, storage);
            B::visit_mir_body_after(tcx, body, storage);
        }

        fn transform_ast(&mut self, krate: &mut ast::Crate) {
            self.first.transform_ast(krate);
            self.second.transform_ast(krate);
        }

        fn transform_mir_body<'tcx>(
            tcx: mir_ty::TyCtxt<'tcx>,
            body: &mut mir::Body<'tcx>,
            storage: &mut dyn Storage,
        ) {
            A::transform_mir_body(tcx, body, storage);
            B::transform_mir_body(tcx, body, storage);
        }
    }

    mod codegen {
        /* NOTE: Currently we do not do any modification during code generation.
         * This wrapper is only supposed to provide callbacks
         */
        use super::*;

        use rustc_codegen_ssa::{
            traits::{CodegenBackend, PrintBackendInfo},
            CodegenResults,
        };
        use rustc_data_structures::fx::FxIndexMap;
        use rustc_metadata::{creader::MetadataLoaderDyn, EncodedMetadata};
        use rustc_middle::util::Providers;
        use rustc_query_system::dep_graph::{WorkProduct, WorkProductId};
        use rustc_session::{
            config::{self, OutputFilenames, PrintRequest},
            Session,
        };
        use rustc_span::{ErrorGuaranteed, Symbol};
        use rustc_target::spec::Target;

        use delegate::delegate;

        pub(super) struct CodegenBackendWrapper<T: ?Sized> {
            backend: Box<dyn CodegenBackend>,
            pass: Arc<Mutex<T>>,
        }

        impl<T: CompilationPass + ?Sized> CodegenBackend for CodegenBackendWrapper<T> {
            delegate! {
                to self.backend {
                    fn locale_resource(&self) -> &'static str;

                    fn init(&self, sess: &Session);
                    fn print(
                        &self,
                        req: &PrintRequest,
                        out: &mut dyn PrintBackendInfo,
                        sess: &Session
                    );
                    fn target_features(&self, sess: &Session, allow_unstable: bool) -> Vec<Symbol>;
                    fn print_passes(&self);
                    fn print_version(&self);

                    fn target_override(
                        &self,
                        opts: &config::Options
                    ) -> Option<Target>;
                    fn metadata_loader(&self) -> Box<MetadataLoaderDyn>;
                    fn provide(&self, providers: &mut Providers);

                    fn join_codegen(
                        &self,
                        ongoing_codegen: Box<dyn Any>,
                        sess: &Session,
                        outputs: &OutputFilenames,
                    ) -> Result<
                        (CodegenResults, FxIndexMap<WorkProductId, WorkProduct>),
                        ErrorGuaranteed,
                    >;

                    fn link(
                        &self,
                        sess: &Session,
                        codegen_results: CodegenResults,
                        outputs: &OutputFilenames,
                    ) -> Result<(), ErrorGuaranteed>;
                }
            }

            fn codegen_crate<'tcx>(
                &self,
                tcx: TyCtxt<'tcx>,
                metadata: EncodedMetadata,
                need_metadata_module: bool,
            ) -> Box<dyn Any> {
                global::with_storage(tcx, |storage| {
                    let mut pass = self.pass.lock().unwrap();
                    pass.visit_tcx_at_codegen_before(tcx, storage)
                });
                let result = self
                    .backend
                    .codegen_crate(tcx, metadata, need_metadata_module);
                global::with_storage(tcx, |storage| {
                    let mut pass = self.pass.lock().unwrap();
                    pass.visit_tcx_at_codegen_after(tcx, storage)
                });
                result
            }
        }

        pub(super) fn get_backend_maker<T: CompilationPass + Send + Sync + ?Sized + 'static>(
            pass: Arc<Mutex<T>>,
        ) -> Box<dyn FnOnce(&config::Options) -> Box<dyn CodegenBackend> + Send> {
            Box::new(|opts| {
                // This is the default implementation taken from `interface::run_compiler`.
                let backend = rustc_interface::util::get_codegen_backend(
                    &rustc_session::EarlyDiagCtxt::new(opts.error_format),
                    &opts.maybe_sysroot,
                    opts.unstable_opts.codegen_backend.as_deref(),
                );
                Box::new(CodegenBackendWrapper { backend, pass })
            })
        }
    }

    mod global {
        use super::*;

        use std::cell::{Cell, RefCell};

        use rustc_middle::ty::TyCtxt;

        thread_local! {
            /*
             * NOTE: We need to provide a global storage for our transformation passes.
             * In the compiler itself, all the shared information is stored in the context.
             * Unless we extend the compiler codes, we cannot put our data there.
             * Thus, we rely on global memory, which is fine for the current state and
             * how the compiler is used. We also have some guards to prevent invalid
             * states that may happen by parallel compilations.
             */
            static CTXT_ID : Cell<Option<usize>> = Cell::new(None);
            static STORAGE: RefCell<HashMap<String, Box<dyn Any>>> = Default::default();
        }

        pub(crate) fn clear_ctxt_id_and_storage() {
            CTXT_ID.replace(None);
            STORAGE.with_borrow_mut(|s| s.clear());
        }

        pub(crate) fn set_ctxt_id(tcx: TyCtxt) {
            let id = get_ctxt_id(tcx);
            let old_id = CTXT_ID.replace(Some(id));
            assert_eq!(
                old_id.unwrap_or(id),
                id,
                "The id has been set before and not cleared. Possibly the thread is being used by other passes."
            );
        }

        pub(crate) fn with_storage<R>(tcx: TyCtxt, f: impl FnOnce(&mut dyn Storage) -> R) -> R {
            assert_eq!(
                CTXT_ID
                    .get()
                    .expect("The id has not been set. Possibly concurrent compilation."),
                get_ctxt_id(tcx),
                "The id is not matched. Possibly concurrent compilation."
            );
            STORAGE.with_borrow_mut(|s| f(s))
        }

        #[inline]
        fn get_ctxt_id(tcx: TyCtxt) -> usize {
            (*std::ops::Deref::deref(&tcx)) as *const _ as usize
        }
    }
}
