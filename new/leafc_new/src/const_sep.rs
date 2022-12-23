
/*
 * Motivation: As we need to report everything to the runtime, we need to add calls to its methods.
 * One of the common things that get passed to the runtime is Operand. 
 * As of the time of writing, it has three types: Copy, Move, and Const.
 * For the
 */
struct ConstSeparator<'tcx> {}