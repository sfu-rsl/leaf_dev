extern crate rustc_middle;
extern crate rustc_span;

use crate::{const_separator, preprocessor};
use leafcommon::{misc, rvalue, switchtargets, ty};
//use log::debug;
use leafcommon::misc::{DebugInfo, PlaceAndDebugInfo};
use rustc_middle::ty::{FnSig, Ty};
use rustc_middle::{
    middle::exported_symbols,
    mir::{
        interpret,
        terminator::{self, TerminatorKind::*},
        BasicBlock, Body, Constant, ConstantKind, LocalDecl, LocalDecls, Operand, Place, Promoted,
        Rvalue, SourceInfo, Statement, StatementKind, VarDebugInfo, VarDebugInfoContents,
    },
    ty::{
        Binder, ConstKind, FloatTy, IntTy, TyCtxt, TyKind, UintTy, Unevaluated, WithOptConstParam,
    },
};
use rustc_span::def_id::DefId;
use std::default::Default;
use std::{collections::HashMap, ops::Index, ops::IndexMut};

pub struct Transformer<'tcx> {
    tcx: TyCtxt<'tcx>,
    func_map: HashMap<String, DefId>,
}

impl<'tcx> Transformer<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>) -> Transformer<'tcx> {
        // Find the leafrt crate and the functions in it.
        // TODO: Perhaps we shouldn't use expect here (and not panic)?
        let cnum = tcx
            .crates(())
            .iter()
            .find(|cnum| tcx.crate_name(**cnum).as_str() == "leafrt")
            .expect("leafrt crate not found");
        let def_ids: Vec<&DefId> = tcx
            .exported_symbols(*cnum)
            .iter()
            .filter_map(|(exported_symbol, _)| match exported_symbol {
                exported_symbols::ExportedSymbol::NonGeneric(def_id)
                | exported_symbols::ExportedSymbol::Generic(def_id, _) => Some(def_id),
                _ => None,
            })
            .collect();

        let mut func_map = HashMap::<String, DefId>::new();
        def_ids.iter().for_each(|def_id| {
            func_map.insert(tcx.def_path_str(**def_id), **def_id);
        });

        Transformer { tcx, func_map }
    }

    pub fn transform(&mut self, body: &mut Body<'tcx>) {
        // Create separate constant assignment statements
        const_separator::separate_consts(body);

        // Clear up the basic_block list
        let mut reverse = preprocessor::clear_and_get_reverse(body);

        // Create a new block for each statement
        let index_counts = preprocessor::repopulate_basic_blocks(body, &mut reverse);

        // Calculate index mappings
        let index_map = preprocessor::create_index_mappings(&index_counts);

        body.basic_blocks().indices().for_each(|basic_block_idx| {
            self.process_terminators(body, basic_block_idx, &index_map);
        });
    }

    fn process_terminators(
        &mut self,
        body: &mut Body<'tcx>,
        basic_block_idx: BasicBlock,
        index_map: &HashMap<BasicBlock, BasicBlock>,
    ) {
        match &body.index(basic_block_idx).terminator {
            Some(_) => {
                // The basic block already has a terminator (it's the last basic block).
                // Adjust jump targets.
                preprocessor::remap_jump_targets(body, basic_block_idx, &index_map);
            }
            None => {
                // A newly-added basic block
                self.add_new_terminator(body, basic_block_idx);
            }
        };
    }

    fn add_new_terminator(&mut self, body: &mut Body<'tcx>, basic_block_idx: BasicBlock) {
        let (_, _, debug_infos) = body.basic_blocks_local_decls_mut_and_var_debug_info();
        let debug_infos = debug_infos.to_owned();

        let new_terminator = Some(if body.index(basic_block_idx).statements.is_empty() {
            let kind = &body
                .basic_blocks()
                .get(basic_block_idx + 1)
                .map(|next_block| next_block.terminator())
                .unwrap()
                .kind;
            match kind {
                SwitchInt {
                    discr,
                    switch_ty: _,
                    targets,
                } => {
                    let discr: rvalue::Operand = discr.into();
                    let targets: switchtargets::SwitchTargets = targets.into();
                    self.build_call_terminator(
                        &mut body.local_decls,
                        basic_block_idx,
                        "leafrt::switch_int",
                        vec![
                            self.build_str(discr.to_string()),
                            self.build_str(targets.to_string()),
                        ],
                    )
                }
                Return => self.build_call_terminator(
                    &mut body.local_decls,
                    basic_block_idx,
                    "leafrt::ret",
                    vec![],
                ),
                Drop {
                    place,
                    target: _,
                    unwind: _,
                } => {
                    let place_and_debug_info =
                        self.build_place_and_debug_info(&debug_infos, Some(place));
                    self.build_call_terminator(
                        &mut body.local_decls,
                        basic_block_idx,
                        "leafrt::drop",
                        vec![self.build_str(place_and_debug_info.to_string())],
                    )
                }
                Call {
                    func,
                    args,
                    destination,
                    cleanup: _,
                    from_hir_call: _,
                    fn_span: _,
                } => {
                    let func_debug_info = self.build_debug_info_function_call(func);
                    let func_return_type: ty::Ty = {
                        match func {
                            Operand::Constant(b) => match b.literal {
                                ConstantKind::Ty(_) => unreachable!(),
                                ConstantKind::Val(_, ty) => match ty.kind() {
                                    TyKind::FnDef(defid, _) => {
                                        let x: Binder<'_, rustc_middle::ty::FnSig<'_>> =
                                            self.tcx.fn_sig(defid);
                                        let x: FnSig = x.skip_binder();
                                        let ty = x.output();
                                        let ty: ty::Ty = ty.into();
                                        ty
                                    }
                                    _ => unreachable!(),
                                },
                            },
                            _ => unreachable!(),
                        }
                    };
                    let func: rvalue::Operand = func.into();
                    // TODO: Make this better?
                    let const_vals = rvalue::OperandConstValueVec(
                        args.iter()
                            .map(|arg| {
                                if let Operand::Constant(c) = arg {
                                    let mut const_as_str = c.to_string();
                                    assert!(const_as_str.starts_with("const "));
                                    for _ in 0.."const ".len() {
                                        const_as_str.remove(0);
                                    }
                                    Some(const_as_str.to_string())
                                    /*
                                    use rustc_middle::ty::print::{FmtPrinter, PrettyPrinter};
                                    let literal = self.tcx.lift(c).unwrap();
                                    let mut cx = FmtPrinter::new(tcx, Namespace::ValueNS);
                                    cx.print_alloc_ids = true;
                                    let cx = cx.pretty_print_const(literal, print_types)?;
                                    Some(cx.into_buffer())
                                     */
                                } else {
                                    None
                                }
                            })
                            .collect(),
                    );
                    let args = rvalue::OperandVec(
                        args.iter().map(|arg| rvalue::Operand::from(arg)).collect(),
                    );

                    let mir_dest = destination.map(|dest| dest.0);
                    let dest_and_debug_info =
                        self.build_place_and_debug_info(&debug_infos, mir_dest.as_ref());

                    self.build_call_terminator(
                        &mut body.local_decls,
                        basic_block_idx,
                        "leafrt::call",
                        vec![
                            self.build_str(func_debug_info.to_string()),
                            self.build_str(func.to_string()),
                            self.build_str(func_return_type.to_string()),
                            self.build_str(args.to_string()),
                            self.build_str(const_vals.to_string()),
                            self.build_str(dest_and_debug_info.to_string()),
                        ],
                    )
                }
                // TODO: Check if we need to handle anything else.
                _ => self.transform_goto(basic_block_idx),
            }
        } else {
            let statement = body
                .index(basic_block_idx)
                .statements
                .first()
                .unwrap()
                .to_owned();
            let kind = statement.kind.to_owned();

            match kind {
                StatementKind::Assign(asgn) => self.transform_assign(
                    &mut body.local_decls,
                    basic_block_idx,
                    &asgn,
                    &debug_infos,
                ),
                // TODO: Check if we need to handle anything else.
                _ => self.transform_goto(basic_block_idx),
            }
        });
        body.index_mut(basic_block_idx).terminator = new_terminator;
    }

    fn transform_assign(
        &mut self,
        local_decls: &mut LocalDecls<'tcx>,
        basic_block_idx: BasicBlock,
        asgn: &Box<(Place<'tcx>, Rvalue<'tcx>)>,
        debug_infos: &[VarDebugInfo],
    ) -> terminator::Terminator<'tcx> {
        let (p, r) = &**asgn;

        let (fn_name, args) = match r {
            Rvalue::Use(op) => self.transform_use(op, &p, &r, &debug_infos),
            _ => {
                let place_and_debug_info = self.build_place_and_debug_info(debug_infos, Some(p));
                let rvalue: rvalue::Rvalue = r.into();
                (
                    "leafrt::assign".into(),
                    vec![
                        self.build_str(place_and_debug_info.to_string()),
                        self.build_str(rvalue.to_string()),
                    ],
                )
            }
        };
        self.build_call_terminator(local_decls, basic_block_idx, &fn_name, args)
    }

    fn transform_use(
        &self,
        op: &Operand<'tcx>,
        p: &Place<'tcx>,
        r: &Rvalue<'tcx>,
        debug_infos: &[VarDebugInfo],
    ) -> (String, Vec<Operand<'tcx>>) {
        let dest_and_debug_info = self.build_place_and_debug_info(debug_infos, Some(p));
        let rvalue: rvalue::Rvalue = r.into();
        if let Some((did, p)) = get_unevaluated_promoted(&op) {
            let promoteds = self.tcx.promoted_mir_opt_const_arg(did);
            let promoted = promoteds.get(p).unwrap();
            // TODO: Not expecting more than one block for a promoted---verify.
            assert!(promoted.basic_blocks().len() == 1);
            let op = promoted
                .basic_blocks()
                .iter()
                .try_for_each(|bb| {
                    // TODO: Not expecting more than one statement with a const for a promoted
                    bb.statements.iter().try_for_each(|statement| {
                        get_const_op(statement).map_or_else(|| Ok(()), |op| Err(op))
                    })
                })
                .unwrap_err();

            (
                get_fn_name(op.constant().unwrap().ty().kind()).unwrap(),
                vec![
                    self.build_str(dest_and_debug_info.to_string()),
                    self.build_str(rvalue.to_string()),
                    (*op).clone(),
                ],
            )
        } else {
            if let Rvalue::Use(Operand::Constant(box c)) = r {
                let fn_name_option = get_fn_name(c.ty().kind());
                if let Some(fn_name) = fn_name_option {
                    return (
                        fn_name,
                        vec![
                            self.build_str(dest_and_debug_info.to_string()),
                            self.build_str(rvalue.to_string()),
                            (*op).clone(),
                        ],
                    );
                }
            }
            (
                "leafrt::assign".into(),
                vec![
                    self.build_str(dest_and_debug_info.to_string()),
                    self.build_str(rvalue.to_string()),
                ],
            )
        }
    }

    fn transform_goto(&self, basic_block_idx: BasicBlock) -> terminator::Terminator<'tcx> {
        terminator::Terminator {
            source_info: SourceInfo::outermost(rustc_span::DUMMY_SP), // TODO: Not sure how good
            kind: Goto {
                target: basic_block_idx + 1,
            },
        }
    }

    fn build_place_and_debug_info(
        &self,
        debug_infos: &[VarDebugInfo],
        place: Option<&Place>,
    ) -> PlaceAndDebugInfo {
        fn build_debug_info_place(debug_infos: &[VarDebugInfo], place: &Place) -> DebugInfo {
            if let Some(debug_info) = debug_infos.iter().find(|info| match info.value {
                VarDebugInfoContents::Place(p) => *place == p,
                _ => false,
            }) {
                debug_info.into()
            } else {
                misc::DebugInfo { name: None }
            }
        }

        place.map_or_else(
            || Default::default(),
            |dest| PlaceAndDebugInfo {
                place: Some(dest.into()),
                debug_info: Some(build_debug_info_place(&debug_infos, &dest)),
            },
        )
    }

    fn build_debug_info_function_call(&self, func_operand: &Operand) -> DebugInfo {
        let constant = if let Operand::Constant(c) = func_operand {
            c
        } else {
            return DebugInfo { name: None };
        };

        match constant.literal {
            ConstantKind::Ty(_) => {}
            ConstantKind::Val(_, ty) => {
                match ty.kind() {
                    TyKind::FnDef(defid, _) => {
                        return DebugInfo {
                            name: Some(self.tcx.def_path_str(*defid)),
                        }
                    }
                    // TODO: Handle these?
                    TyKind::FnPtr(_) => {}
                    TyKind::Closure(_, _) => {}
                    _ => {}
                }
            }
        }

        return DebugInfo { name: None };
    }

    fn build_str(&self, s: String) -> Operand<'tcx> {
        let allocation = interpret::Allocation::from_bytes_byte_aligned_immutable(s.as_bytes());
        let allocation = self.tcx.intern_const_alloc(allocation);
        let span = rustc_span::DUMMY_SP;
        let constant = Constant {
            span: span.to_owned(),
            user_ty: None, // TODO: not sure about this but this is not coming from a user, so...
            literal: ConstantKind::Val(
                interpret::ConstValue::Slice {
                    data: allocation,
                    start: 0,
                    end: s.len(),
                },
                self.tcx.mk_static_str(),
            ),
        };
        Operand::Constant(Box::new(constant))
    }

    fn build_call_terminator(
        &self,
        local_decls: &mut LocalDecls<'tcx>,
        basic_block_idx: BasicBlock,
        func_name: &str,
        args: Vec<Operand<'tcx>>,
    ) -> terminator::Terminator<'tcx> {
        log::debug!("{func_name:?}");
        let def_id = self.func_map.get(func_name).unwrap();
        let ret_local_decl = LocalDecl::new(self.tcx.intern_tup(&[]), rustc_span::DUMMY_SP);
        let ret_local_decl_idx = local_decls.push(ret_local_decl);

        terminator::Terminator {
            source_info: SourceInfo::outermost(rustc_span::DUMMY_SP), // TODO: Not sure how good
            kind: Call {
                func: Operand::function_handle(
                    self.tcx,
                    *def_id,
                    self.tcx.intern_substs(&[]),
                    rustc_span::DUMMY_SP,
                ),
                args,
                destination: Some((Place::from(ret_local_decl_idx), basic_block_idx + 1)),
                cleanup: None,
                from_hir_call: true,
                fn_span: rustc_span::DUMMY_SP,
            },
        }
    }
}

fn get_unevaluated_promoted<'tcx>(
    op: &Operand<'tcx>,
) -> Option<(WithOptConstParam<DefId>, Promoted)> {
    if let Operand::Constant(box Constant {
        span: _,
        user_ty: _,
        literal: ConstantKind::Ty(c),
    }) = op
    {
        if let ConstKind::Unevaluated(Unevaluated {
            def: did,
            substs: _,
            promoted: Some(p),
        }) = c.val()
        {
            return Some((did, p));
        }
    }
    None
}

fn get_const_op<'tcx>(statement: &'tcx Statement<'tcx>) -> Option<&'tcx Operand<'tcx>> {
    if let Statement {
        source_info: _,
        kind:
            StatementKind::Assign(box (
                _,
                Rvalue::Use(
                    op @ Operand::Constant(box Constant {
                        span: _,
                        user_ty: _,
                        literal: _,
                    }),
                ),
            )),
    } = statement
    {
        return Some(op);
    }
    None
}

fn get_fn_name(ty_kind: &TyKind) -> Option<String> {
    let fn_name_suffix = match ty_kind {
        TyKind::Bool => "assign_bool",
        TyKind::Char => "assign_char",
        TyKind::Int(IntTy::Isize) => "assign_isize",
        TyKind::Int(IntTy::I8) => "assign_i8",
        TyKind::Int(IntTy::I16) => "assign_i16",
        TyKind::Int(IntTy::I32) => "assign_i32",
        TyKind::Int(IntTy::I64) => "assign_i64",
        TyKind::Int(IntTy::I128) => "assign_i128",
        TyKind::Uint(UintTy::Usize) => "assign_usize",
        TyKind::Uint(UintTy::U8) => "assign_u8",
        TyKind::Uint(UintTy::U16) => "assign_u16",
        TyKind::Uint(UintTy::U32) => "assign_u32",
        TyKind::Uint(UintTy::U64) => "assign_u64",
        TyKind::Uint(UintTy::U128) => "assign_u128",
        TyKind::Float(FloatTy::F32) => "assign_f32",
        TyKind::Float(FloatTy::F64) => "assign_f64",
        //TyKind::Str => "assign_str",
        TyKind::Ref(_, ty, _) => {
            let tk = ty.kind();
            if let TyKind::Str = tk {
                "assign_str"
            } else {
                return None;
            }
        }
        _ => {
            return None;
        }
    };
    Some(format!("{}{}", "leafrt::", fn_name_suffix))
}
