use rustc_middle::mir::visit::MutVisitor;
use rustc_middle::mir::BasicBlock;
use rustc_middle::mir::BasicBlockData;
use rustc_middle::mir::MirPass;
use rustc_middle::mir::Terminator;
use rustc_middle::mir::TerminatorKind;
use rustc_middle::ty::TyCtxt;

pub struct LeafPass;

impl<'tcx> MirPass<'tcx> for LeafPass {
    fn run_pass(
        &self,
        tcx: rustc_middle::ty::TyCtxt<'tcx>,
        body: &mut rustc_middle::mir::Body<'tcx>,
    ) {
        let mut visitor = LeafVisitor { tcx };
        visitor.visit_body(body);
    }
}

struct LeafVisitor<'tcx> {
    tcx: TyCtxt<'tcx>,
}

impl<'tcx> MutVisitor<'tcx> for LeafVisitor<'tcx> {
    fn tcx(&self) -> TyCtxt<'tcx> {
        self.tcx
    }

    // fn visit_assign(
    //     &mut self,
    //     place: &mut rustc_middle::mir::Place<'tcx>,
    //     rvalue: &mut rustc_middle::mir::Rvalue<'tcx>,
    //     location: rustc_middle::mir::Location,
    // ) {
    //     log::debug!("In assign");
    // }

    fn visit_body(&mut self, body: &mut rustc_middle::mir::Body<'tcx>) {
        let x = body.basic_blocks_mut();
        x.push(BasicBlockData {
            statements: vec![],
            terminator: Some(Terminator {
                kind: TerminatorKind::Unreachable,
                source_info: rustc_middle::mir::SourceInfo::outermost(rustc_span::DUMMY_SP),
            }),
            is_cleanup: false,
        });
    }

    fn visit_assign(&mut self,place: &mut rustc_middle::mir::Place<'tcx>,rvalue: &mut rustc_middle::mir::Rvalue<'tcx>,location:rustc_middle::mir::Location) {
        
    }

    fn visit_place(&mut self,place: &mut rustc_middle::mir::Place<'tcx>,context:rustc_middle::mir::visit::PlaceContext,location:rustc_middle::mir::Location) {
        
    }


    fn visit_basic_block_data(
        &mut self,
        block: BasicBlock,
        data: &mut rustc_middle::mir::BasicBlockData<'tcx>,
    ) {
    }

    fn visit_rvalue(
        &mut self,
        rvalue: &mut rustc_middle::mir::Rvalue<'tcx>,
        location: rustc_middle::mir::Location,
    ) {
        // log::debug!("In rvalue. {:#?}", rvalue);
    }

    fn visit_terminator(
        &mut self,
        terminator: &mut Terminator<'tcx>,
        location: rustc_middle::mir::Location,
    ) {
        
    }
}
