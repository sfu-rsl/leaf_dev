pub trait Runtime : Sized {
    fn assign<T: Assignment>() -> T;

    fn branch<T: Branching>() -> T;

    fn function<T: Function>() -> T;
}

pub trait Assignment {}

pub trait Branching {}

pub trait Function {}
