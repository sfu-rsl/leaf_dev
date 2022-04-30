//pub fn add_this(string: &str) {
//    println!("{string}");
//}
//

pub mod switch_int {
    pub fn filler() {
        println!("switch_int::filler");
    }
}

pub mod ret {
    pub fn filler() {
        println!("ret::filler");
    }
}

pub mod call {
    pub fn filler() {
        println!("call:filler");
    }
}

pub mod assign {
    pub fn filler(arg0: &str, arg1: &str) {
        println!("assign::filler, {arg0}, {arg1}");
    }

    pub mod rvalue {
        pub mod ruse {
            pub fn filler(kind: &str, lvalue: &str, rvalue: &str) {
                println!("assign::rvalue::ruse::filler, {kind}, {lvalue}, {rvalue}");
            }
        }
    }
}
