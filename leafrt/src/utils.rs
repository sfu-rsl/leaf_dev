use std::cmp;

use z3::ast::BV;

trait Extendable {
    fn ensure_size(&self, size: u32) -> Self;
    fn ensure_size_signed(&self, size: u32, signed: bool) -> Self;
}

impl<'ctx> Extendable for BV<'ctx> {
    fn ensure_size(&self, size: u32) -> BV<'ctx> {
        if size > self.get_size() {
            self.sign_ext(size - self.get_size())
        } else {
            self.extract(size - 1, 0)
        }
    }

    fn ensure_size_signed(&self, size: u32, signed: bool) -> BV<'ctx> {
        if size > self.get_size() {
            if signed {
                self.sign_ext(size - self.get_size())
            } else {
                self.zero_ext(size - self.get_size())
            }
        } else {
            self.extract(size - 1, 0)
        }
    }
}

/// Makes sure that both bit-vectors (operands) have the same size and then
/// performs the operation on them. If they do not have the same size, the
/// shorter one will be extended to the length of the longer one.
pub fn bv_safe_binop<'ctx, F, T>(operation: F, left: &BV<'ctx>, right: &BV<'ctx>) -> T
where
    F: FnOnce(&BV<'ctx>, &BV<'ctx>) -> T,
{
    bv_safe_sbinop(operation, left, false, right, false)
}

/// Makes sure that both bit-vectors (operands) have the same size and then
/// performs the operation on them. If they do not have the same size, the
/// shorter one will be extended to the length of the longer one.
pub fn bv_safe_sbinop<'ctx, F, T>(
    operation: F,
    left: &BV<'ctx>,
    left_signed: bool,
    right: &BV<'ctx>,
    right_signed: bool,
) -> T
where
    F: FnOnce(&BV<'ctx>, &BV<'ctx>) -> T,
{
    /* Although Z3 doesn't create new instances if size is not changed, let's
     * have a shortcut for the most common case.
     */
    let left_size = left.get_size();
    let right_size = right.get_size();
    if left_size == right_size && left_signed == right_signed {
        operation(left, right)
    } else {
        let size =
            cmp::max(left_size, right_size) + if left_signed == right_signed { 0 } else { 1 };
        operation(
            &left.ensure_size_signed(size, left_signed),
            &right.ensure_size_signed(size, right_signed),
        )
    }
}
