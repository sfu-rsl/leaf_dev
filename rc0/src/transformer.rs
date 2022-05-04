extern crate rustc_middle;
extern crate rustc_span;

use log;
use rc0lib::place;
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
                exported_symbols::ExportedSymbol::NonGeneric(def_id) => Some(def_id),
                _ => None,
            })
            .collect();

        let mut func_map = collections::HashMap::<String, def_id::DefId>::new();
        for def_id in def_ids.iter() {
            func_map.insert(tcx.def_path_str(**def_id), **def_id);
        }

        Transformer { tcx, func_map }
    }

    pub fn transform(&mut self, body: &mut mir::Body<'tcx>) {
        // First clear up the basic_block list
        let mut reverse = Self::clear_and_get_reverse(body);

        // Create a new block for each statement
        let index_counts = Self::repopulate_basic_blocks(body, &mut reverse);

        // Calculate index mappings
        let index_map = Self::create_index_mappings(&index_counts);

        body.basic_blocks().indices().for_each(|basic_block_idx| {
            self.process_terminators(body, basic_block_idx, &index_map);
        });
    }

    fn clear_and_get_reverse(body: &mut mir::Body<'tcx>) -> Vec<mir::BasicBlockData<'tcx>> {
        let basic_blocks = body.basic_blocks_mut();
        let mut reverse = vec![];
        while let Some(basic_block) = basic_blocks.pop() {
            reverse.push(basic_block);
        }

        reverse
    }

    fn repopulate_basic_blocks(
        body: &mut mir::Body<'tcx>,
        reverse: &mut Vec<mir::BasicBlockData<'tcx>>,
    ) -> Vec<usize> {
        let mut index_counts = vec![];
        while let Some(basic_block) = reverse.pop() {
            Self::create_new_blocks(body, basic_block, &mut index_counts);
        }

        index_counts
    }

    fn create_new_blocks(
        body: &mut mir::Body<'tcx>,
        mut basic_block: mir::BasicBlockData<'tcx>,
        index_counts: &mut Vec<usize>,
    ) {
        let mut new_basic_blocks = vec![];

        // # of statements + block for teminator call + terminator
        // We do this before we pop basic_block.statements.
        index_counts.push(basic_block.statements.len() + 2);

        // Create a new block with the original terminator and no statements.
        let mut new_basic_block = mir::BasicBlockData::new(basic_block.terminator);
        new_basic_block.is_cleanup = basic_block.is_cleanup;
        new_basic_blocks.push(new_basic_block);

        // Create a new block that will include a rc0lib call for the terminator
        new_basic_blocks.push(mir::BasicBlockData::new(None));

        while let Some(statement) = basic_block.statements.pop() {
            // Process from the last statement in the block
            let mut new_basic_block = mir::BasicBlockData::new(None);
            new_basic_block.statements = Vec::from([statement]);
            new_basic_blocks.push(new_basic_block);
        }

        let basic_blocks = body.basic_blocks_mut();
        while let Some(new_basic_block) = new_basic_blocks.pop() {
            basic_blocks.push(new_basic_block);
        }
    }

    fn create_index_mappings(
        index_counts: &Vec<usize>,
    ) -> collections::HashMap<mir::BasicBlock, mir::BasicBlock> {
        let mut index_map = collections::HashMap::<mir::BasicBlock, mir::BasicBlock>::new();
        let mut origin_index: usize = 0;
        let mut new_index = 0;

        for cnt in index_counts {
            // At this point, index_counts contains a list of how many basic blocks replace each of
            // the original basic blocks
            index_map.insert(
                mir::BasicBlock::from_usize(origin_index),
                mir::BasicBlock::from_usize(new_index),
            );
            origin_index += 1;
            new_index += cnt;
        }

        index_map
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
                Self::remap_jump_targets(body, basic_block_idx, &index_map);
            }
            None => {
                // A newly-added basic block
                self.add_new_terminator(body, basic_block_idx);
            }
        };
    }

    fn remap_jump_targets(
        body: &mut mir::Body<'tcx>,
        basic_block_idx: mir::BasicBlock,
        index_map: &collections::HashMap<mir::BasicBlock, mir::BasicBlock>,
    ) {
        // Adjust jump targets
        match &mut body.index_mut(basic_block_idx).terminator_mut().kind {
            Goto { target } => {
                // target has to exist in index_map
                *target = index_map.get(&target).unwrap().to_owned();
            }
            SwitchInt {
                discr: _,     //: Operand<'tcx>,
                switch_ty: _, //: Ty<'tcx>,
                targets,      //: SwitchTargets,
            } => {
                for target in targets.all_targets_mut() {
                    *target = index_map.get(&target).unwrap().to_owned();
                }
            }
            ////Resume,
            ////Abort,
            ////Return,
            ////Unreachable,
            Drop {
                place: _, //: Place<'tcx>,
                target,   //: BasicBlock,
                unwind,   //: Option<BasicBlock>,
            } => {
                *target = index_map.get(&target).unwrap().to_owned();
                *unwind = unwind.map(|idx| index_map.get(&idx).unwrap().to_owned());
            }
            DropAndReplace {
                place: _, //: Place<'tcx>,
                value: _, //: Operand<'tcx>,
                target,   //: BasicBlock,
                unwind,   //: Option<BasicBlock>,
            } => {
                *target = index_map.get(&target).unwrap().to_owned();
                *unwind = unwind.map(|idx| index_map.get(&idx).unwrap().to_owned());
            }
            Call {
                func: _,          //: Operand<'tcx>,
                args: _,          //: Vec<Operand<'tcx>>,
                destination,      //: Option<(Place<'tcx>, BasicBlock)>,
                cleanup,          //: Option<BasicBlock>,
                from_hir_call: _, //: bool,
                fn_span: _,       //: Span,
            } => {
                *destination = destination
                    .map(|(place, idx)| (place, index_map.get(&idx).unwrap().to_owned()));
                *cleanup = cleanup.map(|idx| index_map.get(&idx).unwrap().to_owned());
            }
            Assert {
                cond: _,     //: Operand<'tcx>,
                expected: _, //: bool,
                msg: _,      //: AssertMessage<'tcx>,
                target,      //: BasicBlock,
                cleanup,     //: Option<BasicBlock>,
            } => {
                *target = index_map.get(&target).unwrap().to_owned();
                *cleanup = cleanup.map(|idx| index_map.get(&idx).unwrap().to_owned());
            }
            Yield {
                value: _,      //: Operand<'tcx>,
                resume,        //: BasicBlock,
                resume_arg: _, //: Place<'tcx>,
                drop,          //: Option<BasicBlock>,
            } => {
                *resume = index_map.get(&resume).unwrap().to_owned();
                *drop = drop.map(|idx| index_map.get(&idx).unwrap().to_owned());
            }
            //GeneratorDrop,
            FalseEdge {
                real_target,      //: BasicBlock,
                imaginary_target, //: BasicBlock,
            } => {
                *real_target = index_map.get(&real_target).unwrap().to_owned();
                *imaginary_target = index_map.get(&imaginary_target).unwrap().to_owned();
            }
            FalseUnwind {
                real_target, //: BasicBlock,
                unwind,      //: Option<BasicBlock>,
            } => {
                *real_target = index_map.get(&real_target).unwrap().to_owned();
                *unwind = unwind.map(|idx| index_map.get(&idx).unwrap().to_owned());
            }
            InlineAsm {
                template: _,   //: &'tcx [InlineAsmTemplatePiece],
                operands: _,   //: Vec<InlineAsmOperand<'tcx>>,
                options: _,    //: InlineAsmOptions,
                line_spans: _, //: &'tcx [Span],
                destination,   //: Option<BasicBlock>,
                cleanup,       //: Option<BasicBlock>,
            } => {
                *destination = destination.map(|idx| index_map.get(&idx).unwrap().to_owned());
                *cleanup = cleanup.map(|idx| index_map.get(&idx).unwrap().to_owned());
            }
            _ => (),
        }
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
                } => self.handle_switch_int(&mut body.local_decls, basic_block_idx),
                Return => self.handle_ret(&mut body.local_decls, basic_block_idx),
                Call {
                    func: _,
                    args: _,
                    destination: _,
                    cleanup: _,
                    from_hir_call: _,
                    fn_span: _,
                } => self.handle_call(&mut body.local_decls, basic_block_idx),
                // TODO: Check if we need to handle anything else.
                _ => self.handle_goto(basic_block_idx),
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
                    self.handle_assign(&mut body.local_decls, basic_block_idx, asgn)
                }
                // TODO: Check if we need to handle anything else.
                _ => self.handle_goto(basic_block_idx),
            }
        });
        body.index_mut(basic_block_idx).terminator = new_terminator;
    }

    // TODO: Filler
    fn handle_switch_int(
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
    fn handle_ret(
        &mut self,
        local_decls: &mut mir::LocalDecls<'tcx>,
        basic_block_idx: mir::BasicBlock,
    ) -> terminator::Terminator<'tcx> {
        self.build_call_terminator(local_decls, basic_block_idx, "rc0lib::ret::filler", vec![])
    }

    // TODO: Filler
    fn handle_call(
        &mut self,
        local_decls: &mut mir::LocalDecls<'tcx>,
        basic_block_idx: mir::BasicBlock,
    ) -> terminator::Terminator<'tcx> {
        self.build_call_terminator(local_decls, basic_block_idx, "rc0lib::call::filler", vec![])
    }

    fn handle_assign(
        &mut self,
        local_decls: &mut mir::LocalDecls<'tcx>,
        basic_block_idx: mir::BasicBlock,
        asgn: &Box<(mir::Place<'tcx>, mir::Rvalue<'tcx>)>,
    ) -> terminator::Terminator<'tcx> {
        let (place, rvalue) = asgn.as_ref();
        self.build_call_terminator(
            local_decls,
            basic_block_idx,
            "rc0lib::assign::serialize",
            vec![self.build_str(serde_json::to_string(&place::Place::from(place)).unwrap())],
        )
    }

    fn handle_goto(&self, basic_block_idx: mir::BasicBlock) -> terminator::Terminator<'tcx> {
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
