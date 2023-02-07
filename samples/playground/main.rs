fn main() {
    let y = [1 as u128, 2, 3];
    get_slice(&y);
}

fn get_slice(s: &[u128]) {

}

fn get_num() -> u32 {
    10
}

fn test(x: BranchingInfo) {

}


pub type Local = u32;

pub type Ref = u64;
pub type PlaceRef = Ref;
pub type OperandRef = Ref;

pub type VariantIndex = u32;


pub type BasicBlockIndex = u32;
pub type BranchTarget = BasicBlockIndex;

pub struct BranchingInfo {
    pub node_location: BasicBlockIndex,
    pub discriminant: OperandRef,
}

impl BranchingInfo {
    pub fn new(node_location: BasicBlockIndex, discriminant: OperandRef) -> Self {
        Self {
            node_location,
            discriminant,
        }
    }
}
