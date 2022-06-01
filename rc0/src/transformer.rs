extern crate rustc_middle;
extern crate rustc_span;

use crate::helpers;
use log::debug;
use rc0common::{place, rvalue};
use rustc_middle::{
    middle::exported_symbols,
    mir::{
        self, interpret,
        terminator::{self, TerminatorKind::*},
    },
    ty,
};
use rustc_span::def_id;
use std::{collections, ops::Index, ops::IndexMut};

pub struct Transformer<'tcx> {
    tcx: ty::TyCtxt<'tcx>,
    func_map: collections::HashMap<String, def_id::DefId>,
}

impl<'tcx> Transformer<'tcx> {
    pub fn new(tcx: ty::TyCtxt<'tcx>) -> Transformer<'tcx> {
        // Find the rc0lib crate and the functions in it.
        // TODO: Perhaps we shouldn't use expect here (and not panic)?
        let cnum = tcx
            .crates(())
            .iter()
            .find(|cnum| tcx.crate_name(**cnum).as_str() == "rc0lib")
            .expect("rc0lib crate not found");
        let def_ids: Vec<&def_id::DefId> = tcx
            .exported_symbols(*cnum)
            .iter()
            .filter_map(|(exported_symbol, _)| match exported_symbol {
                exported_symbols::ExportedSymbol::NonGeneric(def_id)
                | exported_symbols::ExportedSymbol::Generic(def_id, _) => Some(def_id),
                _ => None,
            })
            .collect();

        let mut func_map = collections::HashMap::<String, def_id::DefId>::new();
        def_ids.iter().for_each(|def_id| {
            func_map.insert(tcx.def_path_str(**def_id), **def_id);
        });

        Transformer { tcx, func_map }
    }

    pub fn transform(&mut self, body: &mut mir::Body<'tcx>) {
        // Create separate constant assignment statements
        helpers::separate_consts(body);

        // Clear up the basic_block list
        let mut reverse = helpers::clear_and_get_reverse(body);

        // Create a new block for each statement
        let index_counts = helpers::repopulate_basic_blocks(body, &mut reverse);

        // Calculate index mappings
        let index_map = helpers::create_index_mappings(&index_counts);

        body.basic_blocks().indices().for_each(|basic_block_idx| {
            self.process_terminators(body, basic_block_idx, &index_map);
        });
    }

    fn process_terminators(
        &mut self,
        body: &mut mir::Body<'tcx>,
        basic_block_idx: mir::BasicBlock,
        index_map: &collections::HashMap<mir::BasicBlock, mir::BasicBlock>,
    ) {
        match &body.index(basic_block_idx).terminator {
            Some(_) => {
                // The basic block already has a terminator (it's the last basic block).
                // Adjust jump targets.
                helpers::remap_jump_targets(body, basic_block_idx, &index_map);
            }
            None => {
                // A newly-added basic block
                self.add_new_terminator(body, basic_block_idx);
            }
        };
    }

    fn add_new_terminator(&mut self, body: &mut mir::Body<'tcx>, basic_block_idx: mir::BasicBlock) {
        let new_terminator = Some(if body.index(basic_block_idx).statements.is_empty() {
            let kind = &body
                .basic_blocks()
                .get(basic_block_idx + 1)
                .map(|next_block| next_block.terminator())
                .unwrap()
                .kind;
            match kind {
                SwitchInt {
                    discr: _,
                    switch_ty: _,
                    targets: _,
                } => self.transform_switch_int(&mut body.local_decls, basic_block_idx),
                Return => self.transform_ret(&mut body.local_decls, basic_block_idx),
                Call {
                    func: _,
                    args: _,
                    destination: _,
                    cleanup: _,
                    from_hir_call: _,
                    fn_span: _,
                } => self.transform_call(&mut body.local_decls, basic_block_idx),
                // TODO: Check if we need to handle anything else.
                _ => self.transform_goto(basic_block_idx),
            }
        } else {
            let kind = &body
                .index(basic_block_idx)
                .statements
                .first()
                .unwrap()
                .kind
                .to_owned();
            match kind {
                mir::StatementKind::Assign(asgn) => {
                    self.transform_assign(&mut body.local_decls, basic_block_idx, asgn)
                }
                // TODO: Check if we need to handle anything else.
                _ => self.transform_goto(basic_block_idx),
            }
        });
        body.index_mut(basic_block_idx).terminator = new_terminator;
    }

    // TODO: Filler
    fn transform_switch_int(
        &mut self,
        local_decls: &mut mir::LocalDecls<'tcx>,
        basic_block_idx: mir::BasicBlock,
    ) -> terminator::Terminator<'tcx> {
        self.build_call_terminator(
            local_decls,
            basic_block_idx,
            "rc0lib::switch_int::filler",
            vec![],
        )
    }

    // TODO: Filler
    fn transform_ret(
        &mut self,
        local_decls: &mut mir::LocalDecls<'tcx>,
        basic_block_idx: mir::BasicBlock,
    ) -> terminator::Terminator<'tcx> {
        self.build_call_terminator(local_decls, basic_block_idx, "rc0lib::ret::filler", vec![])
    }

    // TODO: Filler
    fn transform_call(
        &mut self,
        local_decls: &mut mir::LocalDecls<'tcx>,
        basic_block_idx: mir::BasicBlock,
    ) -> terminator::Terminator<'tcx> {
        self.build_call_terminator(local_decls, basic_block_idx, "rc0lib::call::filler", vec![])
    }

    fn transform_assign(
        &mut self,
        local_decls: &mut mir::LocalDecls<'tcx>,
        basic_block_idx: mir::BasicBlock,
        asgn: &Box<(mir::Place<'tcx>, mir::Rvalue<'tcx>)>,
    ) -> terminator::Terminator<'tcx> {
        debug!("Assign: {asgn:?}");
        let (p, r) = &**asgn;
        let (fn_name, args) = match r {
            mir::Rvalue::Use(op) => self.transform_use(op, &p, &r),
            _ => {
                debug!("Rvalue: Other");
                let place: place::Place = p.into();
                let rvalue: rvalue::Rvalue = r.into();
                (
                    "rc0lib::assign::deserialize".into(),
                    vec![
                        self.build_str(place.to_string()),
                        self.build_str(rvalue.to_string()),
                    ],
                )
            }
        };
        self.build_call_terminator(local_decls, basic_block_idx, &fn_name, args)
    }

    fn transform_use(
        &self,
        op: &mir::Operand<'tcx>,
        p: &mir::Place<'tcx>,
        r: &mir::Rvalue<'tcx>,
    ) -> (String, Vec<mir::Operand<'tcx>>) {
        debug!("Rvalue: Use");
        let place: place::Place = p.into();
        let rvalue: rvalue::Rvalue = r.into();
        if let Some((did, p)) = helpers::get_unevaluated_promoted(&op) {
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
                        debug!("statement: {statement:?}");
                        helpers::get_const_op(statement).map_or_else(|| Ok(()), |op| Err(op))
                    })
                })
                .unwrap_err();

            (
                helpers::get_fn_name(op.constant().unwrap().ty().kind()).unwrap(),
                vec![
                    self.build_str(place.to_string()),
                    self.build_str(rvalue.to_string()),
                    (*op).clone(),
                ],
            )
        } else {
            if let mir::Rvalue::Use(mir::Operand::Constant(box c)) = r {
                let fn_name_option = helpers::get_fn_name(c.ty().kind());
                if let Some(fn_name) = fn_name_option {
                    return (
                        fn_name,
                        vec![
                            self.build_str(place.to_string()),
                            self.build_str(rvalue.to_string()),
                            //mir::Operand::Copy((*p).into()),
                            (*op).clone(),
                        ],
                    );
                }
            }
            (
                "rc0lib::assign::deserialize".into(),
                vec![
                    self.build_str(place.to_string()),
                    self.build_str(rvalue.to_string()),
                ],
            )
        }
    }

    fn transform_goto(&self, basic_block_idx: mir::BasicBlock) -> terminator::Terminator<'tcx> {
        terminator::Terminator {
            source_info: mir::SourceInfo::outermost(rustc_span::DUMMY_SP), // TODO: Not sure how good
            kind: Goto {
                target: basic_block_idx + 1,
            },
        }
    }

    fn build_str(&self, s: String) -> mir::Operand<'tcx> {
        let allocation = interpret::Allocation::from_bytes_byte_aligned_immutable(s.as_bytes());
        let allocation = self.tcx.intern_const_alloc(allocation);
        let span = rustc_span::DUMMY_SP;
        let constant = mir::Constant {
            span: span.to_owned(),
            user_ty: None, // TODO: not sure about this but this is not coming from a user, so...
            literal: mir::ConstantKind::Val(
                interpret::ConstValue::Slice {
                    data: allocation,
                    start: 0,
                    end: s.len(),
                },
                self.tcx.mk_static_str(),
            ),
        };
        mir::Operand::Constant(Box::new(constant))
    }

    fn build_call_terminator(
        &self,
        local_decls: &mut mir::LocalDecls<'tcx>,
        basic_block_idx: mir::BasicBlock,
        func_name: &str,
        args: Vec<mir::Operand<'tcx>>,
    ) -> terminator::Terminator<'tcx> {
        let def_id = self.func_map.get(func_name).unwrap();
        let ret_local_decl = mir::LocalDecl::new(self.tcx.intern_tup(&[]), rustc_span::DUMMY_SP);
        let ret_local_decl_idx = local_decls.push(ret_local_decl);

        terminator::Terminator {
            source_info: mir::SourceInfo::outermost(rustc_span::DUMMY_SP), // TODO: Not sure how good
            kind: Call {
                func: mir::Operand::function_handle(
                    self.tcx,
                    *def_id,
                    self.tcx.intern_substs(&[]),
                    rustc_span::DUMMY_SP,
                ),
                args,
                destination: Some((mir::Place::from(ret_local_decl_idx), basic_block_idx + 1)),
                cleanup: None,
                from_hir_call: true,
                fn_span: rustc_span::DUMMY_SP,
            },
        }
    }
}
