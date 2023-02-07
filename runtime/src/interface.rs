trait RuntimeInterface {
    fn assign_use();
    fn assign_repeat();
    fn assign_ref();
    fn assign_thread_local_ref();
    fn assign_address_of();
    fn assign_len();    // To be investigated. Not obvious whether it appears at all in the later stages.
    fn assign_cast();

    fn assign_binary_op(checked: bool);
    fn assign_nullary_op(); // May be converted to other operations
    fn assign_unary_op();

    fn assign_discriminant();

    fn assign_aggregate();

    fn assign_shallow_init_box();

    fn switch_int();

    fn fn_call();
    fn fn_return();
    fn fn_yield(); // Like return
    fn resume(); // For context switches
}