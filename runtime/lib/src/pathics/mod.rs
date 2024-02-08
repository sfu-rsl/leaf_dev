use crate::abs::backend::PathInterestChecker;

pub(crate) struct AllPathInterestChecker;

impl<S> PathInterestChecker<S> for AllPathInterestChecker {
    fn is_interesting(&self, _path: &[S]) -> bool {
        true
    }
}
