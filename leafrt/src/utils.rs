use std::cmp;

use z3::ast::BV;

trait Extendable {
    fn ensure_size(&self, size: u32) -> Self;
}

impl<'ctx> Extendable for BV<'ctx> {
    fn ensure_size(&self, size: u32) -> BV<'ctx> {
        if size > self.get_size() {
            self.sign_ext(size - self.get_size())
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
    /* Although Z3 doesn't create new instances if size is not changed, let's
     * have a shortcut for the most common case.
     */
    if left.get_size() == right.get_size() {
        operation(left, right)
    } else {
        let size = cmp::max(left.get_size(), right.get_size());
        operation(&left.ensure_size(size), &right.ensure_size(size))
    }
}
