use rustc_ast as ast;
use rustc_driver as driver;
use rustc_interface::{interface, Queries};
use rustc_middle::mir;
use rustc_middle::ty as mir_ty;

mod instr;

pub(crate) trait CompilationPass {
    fn get_callbacks<'a>(&'a mut self) -> &'a mut (dyn driver::Callbacks + Send);
}

impl<T: driver::Callbacks + Send> CompilationPass for Box<T> {
    fn get_callbacks<'a>(&'a mut self) -> &'a mut (dyn driver::Callbacks + Send) {
        self.as_mut()
    }
}

#[allow(unused)]
pub(crate) trait AnalysisPass {
    fn visit_ast(&mut self, krate: &ast::Crate) {
        Default::default()
    }

    fn visit_ctxt<'tcx>(&mut self, tcx: mir_ty::TyCtxt<'tcx>) {
        Default::default()
    }

    fn visit_mir_body<'tcx>(&mut self, tcx: mir_ty::TyCtxt<'tcx>, body: &mir::Body<'tcx>) {
        Default::default()
    }
}

trait AnalysisPassExt {
    fn as_compilation<'a>(&'a mut self) -> Box<dyn CompilationPass + 'a>;
}
impl<T: AnalysisPass + Send> AnalysisPassExt for T {
    fn as_compilation<'a>(&'a mut self) -> Box<dyn CompilationPass + 'a> {
        Box::new(Box::new(implementation::AnalysisPassAdapter(self)))
    }
}

#[allow(unused)]
pub(crate) trait TransformationPass {
    fn transform_ast(&mut self, krate: &mut ast::Crate) {
        Default::default()
    }

    /* As we need function pointers in the query structures, this function cannot
     * take self. */
    fn transform_mir_body<'tcx>(tcx: mir_ty::TyCtxt<'tcx>, body: &mut mir::Body<'tcx>) {
        Default::default()
    }
}

trait TransformationPassExt {
    fn as_compilation<'a>(&'a mut self) -> Box<dyn CompilationPass + 'a>;
}
impl<T: TransformationPass + Send> TransformationPassExt for T {
    fn as_compilation<'a>(&'a mut self) -> Box<dyn CompilationPass + 'a> {
        Box::new(Box::new(implementation::TransformationPassAdapter(self)))
    }
}

mod implementation {
    use super::*;
    use mir_ty::TyCtxt;
    use rustc_driver::Compilation;
    use rustc_middle::query;
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
     */

    pub(super) struct AnalysisPassAdapter<'a, T: AnalysisPass + Send>(pub &'a mut T);

    impl<T> driver::Callbacks for AnalysisPassAdapter<'_, T>
    where
        T: AnalysisPass + Send,
    {
        fn config(&mut self, _config: &mut interface::Config) {}

        fn after_parsing<'tcx>(
            &mut self,
            _compiler: &interface::Compiler,
            queries: &'tcx Queries<'tcx>,
        ) -> Compilation {
            self.0.visit_ast(&queries.parse().unwrap().borrow());
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
            _handler: &rustc_session::EarlyErrorHandler,
            _compiler: &interface::Compiler,
            queries: &'tcx Queries<'tcx>,
        ) -> Compilation {
            queries
                .global_ctxt()
                .unwrap()
                .enter(|tcx| self.0.visit_ctxt(tcx));
            Compilation::Continue
        }
    }

    thread_local! {
        /*
         * NOTE: Both `override_queries` and `optimized_mir` are function pointers, which means
         * that they cannot be closures with captured variables. Therefore, we use these statically
         * allocated cells to store the original functions.
         * As there will be a single transformation pass in the project, this is not a problem.
         */
        static ORIGINAL_OVERRIDE: Cell<
            Option<fn(&rustc_session::Session, &mut query::Providers, &mut query::ExternProviders)>,
        > = Cell::new(None);
        static ORIGINAL_OPTIMIZED_MIR: Cell<
            for<'tcx> fn(TyCtxt<'tcx>, LocalDefId) -> &mir::Body<'tcx>
        > = Cell::new(|_, _| unreachable!());
    }

    pub(super) struct TransformationPassAdapter<'a, T: TransformationPass + Send>(pub &'a mut T);

    impl<T> driver::Callbacks for TransformationPassAdapter<'_, T>
    where
        T: TransformationPass + Send,
    {
        fn config(&mut self, config: &mut interface::Config) {
            ORIGINAL_OVERRIDE.set(config.override_queries.take());

            config.override_queries = Some(move |session, providers, e_providers| {
                if let Some(existing_override) = ORIGINAL_OVERRIDE.get() {
                    existing_override(session, providers, e_providers);
                }

                ORIGINAL_OPTIMIZED_MIR.set(providers.optimized_mir);
                providers.optimized_mir = Self::optimized_mir;
            });
        }

        fn after_parsing<'tcx>(
            &mut self,
            _compiler: &interface::Compiler,
            queries: &'tcx Queries<'tcx>,
        ) -> Compilation {
            self.0.transform_ast(queries.parse().unwrap().get_mut());
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
            _handler: &rustc_session::EarlyErrorHandler,
            _compiler: &interface::Compiler,
            _queries: &'tcx Queries<'tcx>,
        ) -> Compilation {
            Compilation::Continue
        }
    }

    impl<T: TransformationPass + Send> TransformationPassAdapter<'_, T> {
        fn optimized_mir<'tcx>(tcx: TyCtxt<'tcx>, id: LocalDefId) -> &mir::Body {
            /* NOTE: Currently, it seems that there is no way to deallocate
             * something from arena. So, we have to the body. */
            let mut body = ORIGINAL_OPTIMIZED_MIR.get()(tcx, id).clone();
            T::transform_mir_body(tcx, &mut body);
            tcx.arena.alloc(body)
        }
    }
}
