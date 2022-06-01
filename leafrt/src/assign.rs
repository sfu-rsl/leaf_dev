use leafcommon::place::Place;
use leafcommon::rvalue::Rvalue;

pub fn deserialize(place: &str, rvalue: &str) {
    println!("deserialize");
    let place: Place = place.try_into().unwrap();
    println!("{place:?}");

    let rvalue: Rvalue = rvalue.try_into().unwrap();
    println!("{rvalue:?}");
}

pub fn const_isize(place: &str, rvalue: &str, constant: isize) {
    println!("const_isize");
    let place: Place = place.try_into().unwrap();
    println!("{place:?}");

    let rvalue: Rvalue = rvalue.try_into().unwrap();
    println!("{rvalue:?}");

    println!("{constant:?}");
}

pub fn const_i8(place: &str, rvalue: &str, constant: i8) {
    println!("const_i8");
    let place: Place = place.try_into().unwrap();
    println!("{place:?}");

    let rvalue: Rvalue = rvalue.try_into().unwrap();
    println!("{rvalue:?}");

    println!("{constant:?}");
}

pub fn const_i16(place: &str, rvalue: &str, constant: i16) {
    println!("const_i16");
    let place: Place = place.try_into().unwrap();
    println!("{place:?}");

    let rvalue: Rvalue = rvalue.try_into().unwrap();
    println!("{rvalue:?}");

    println!("{constant:?}");
}

pub fn const_i32(place: &str, rvalue: &str, constant: i32) {
    println!("const_i32");
    let place: Place = place.try_into().unwrap();
    println!("{place:?}");

    let rvalue: Rvalue = rvalue.try_into().unwrap();
    println!("{rvalue:?}");

    println!("{constant:?}");
}

pub fn const_i64(place: &str, rvalue: &str, constant: i64) {
    println!("const_i64");
    let place: Place = place.try_into().unwrap();
    println!("{place:?}");

    let rvalue: Rvalue = rvalue.try_into().unwrap();
    println!("{rvalue:?}");

    println!("{constant:?}");
}

pub fn const_i128(place: &str, rvalue: &str, constant: i128) {
    println!("const_i128");
    let place: Place = place.try_into().unwrap();
    println!("{place:?}");

    let rvalue: Rvalue = rvalue.try_into().unwrap();
    println!("{rvalue:?}");

    println!("{constant:?}");
}

pub fn const_usize(place: &str, rvalue: &str, constant: usize) {
    println!("const_usize");
    let place: Place = place.try_into().unwrap();
    println!("{place:?}");

    let rvalue: Rvalue = rvalue.try_into().unwrap();
    println!("{rvalue:?}");

    println!("{constant:?}");
}

pub fn const_u8(place: &str, rvalue: &str, constant: u8) {
    println!("const_u8");
    let place: Place = place.try_into().unwrap();
    println!("{place:?}");

    let rvalue: Rvalue = rvalue.try_into().unwrap();
    println!("{rvalue:?}");

    println!("{constant:?}");
}

pub fn const_u16(place: &str, rvalue: &str, constant: u16) {
    println!("const_u16");
    let place: Place = place.try_into().unwrap();
    println!("{place:?}");

    let rvalue: Rvalue = rvalue.try_into().unwrap();
    println!("{rvalue:?}");

    println!("{constant:?}");
}

pub fn const_u32(place: &str, rvalue: &str, constant: u32) {
    println!("const_u32");
    let place: Place = place.try_into().unwrap();
    println!("{place:?}");

    let rvalue: Rvalue = rvalue.try_into().unwrap();
    println!("{rvalue:?}");

    println!("{constant:?}");
}

pub fn const_u64(place: &str, rvalue: &str, constant: u64) {
    println!("const_u64");
    let place: Place = place.try_into().unwrap();
    println!("{place:?}");

    let rvalue: Rvalue = rvalue.try_into().unwrap();
    println!("{rvalue:?}");

    println!("{constant:?}");
}

pub fn const_u128(place: &str, rvalue: &str, constant: u128) {
    println!("const_u128");
    let place: Place = place.try_into().unwrap();
    println!("{place:?}");

    let rvalue: Rvalue = rvalue.try_into().unwrap();
    println!("{rvalue:?}");

    println!("{constant:?}");
}

pub fn const_f32(place: &str, rvalue: &str, constant: f32) {
    println!("const_f32");
    let place: Place = place.try_into().unwrap();
    println!("{place:?}");

    let rvalue: Rvalue = rvalue.try_into().unwrap();
    println!("{rvalue:?}");

    println!("{constant:?}");
}

pub fn const_f64(place: &str, rvalue: &str, constant: f64) {
    println!("const_f64");
    let place: Place = place.try_into().unwrap();
    println!("{place:?}");

    let rvalue: Rvalue = rvalue.try_into().unwrap();
    println!("{rvalue:?}");

    println!("{constant:?}");
}

pub fn const_char(place: &str, rvalue: &str, constant: char) {
    println!("const_char");
    let place: Place = place.try_into().unwrap();
    println!("{place:?}");

    let rvalue: Rvalue = rvalue.try_into().unwrap();
    println!("{rvalue:?}");

    println!("{constant:?}");
}

pub fn const_bool(place: &str, rvalue: &str, constant: bool) {
    println!("const_bool");
    let place: Place = place.try_into().unwrap();
    println!("{place:?}");

    let rvalue: Rvalue = rvalue.try_into().unwrap();
    println!("{rvalue:?}");

    println!("{constant:?}");
}

pub fn const_str(place: &str, rvalue: &str, constant: &str) {
    println!("const_str");
    let place: Place = place.try_into().unwrap();
    println!("{place:?}");

    let rvalue: Rvalue = rvalue.try_into().unwrap();
    println!("{rvalue:?}");

    println!("{constant:?}");
}

//pub fn ref_isize(place: &str, rvalue: &str, constant: &isize) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_i8(place: &str, rvalue: &str, constant: &i8) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_i16(place: &str, rvalue: &str, constant: &i16) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_i32(place: &str, rvalue: &str, constant: &i32) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_i64(place: &str, rvalue: &str, constant: &i64) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_i128(place: &str, rvalue: &str, constant: &i128) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_usize(place: &str, rvalue: &str, constant: &usize) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_u8(place: &str, rvalue: &str, constant: &u8) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_u16(place: &str, rvalue: &str, constant: &u16) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_u32(place: &str, rvalue: &str, constant: &u32) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_u64(place: &str, rvalue: &str, constant: &u64) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_u128(place: &str, rvalue: &str, constant: &u128) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_f32(place: &str, rvalue: &str, constant: &f32) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_f64(place: &str, rvalue: &str, constant: &f64) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_char(place: &str, rvalue: &str, constant: &char) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_bool(place: &str, rvalue: &str, constant: &bool) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_mut_isize(place: &str, rvalue: &str, constant: &mut isize) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_mut_i8(place: &str, rvalue: &str, constant: &mut i8) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_mut_i16(place: &str, rvalue: &str, constant: &mut i16) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_mut_i32(place: &str, rvalue: &str, constant: &mut i32) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_mut_i64(place: &str, rvalue: &str, constant: &mut i64) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_mut_i128(place: &str, rvalue: &str, constant: &mut i128) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_mut_usize(place: &str, rvalue: &str, constant: &mut usize) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_mut_u8(place: &str, rvalue: &str, constant: &mut u8) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_mut_u16(place: &str, rvalue: &str, constant: &mut u16) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_mut_u32(place: &str, rvalue: &str, constant: &mut u32) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_mut_u64(place: &str, rvalue: &str, constant: &mut u64) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_mut_u128(place: &str, rvalue: &str, constant: &mut u128) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_mut_f32(place: &str, rvalue: &str, constant: &mut f32) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_mut_f64(place: &str, rvalue: &str, constant: &mut f64) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_mut_char(place: &str, rvalue: &str, constant: &mut char) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ref_mut_bool(place: &str, rvalue: &str, constant: &mut bool) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ptr_isize(place: &str, rvalue: &str, constant: isize) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ptr_i8(place: &str, rvalue: &str, constant: i8) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ptr_i16(place: &str, rvalue: &str, constant: i16) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ptr_i32(place: &str, rvalue: &str, constant: i32) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ptr_i64(place: &str, rvalue: &str, constant: i64) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ptr_i128(place: &str, rvalue: &str, constant: i128) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ptr_usize(place: &str, rvalue: &str, constant: usize) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ptr_u8(place: &str, rvalue: &str, constant: u8) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ptr_u16(place: &str, rvalue: &str, constant: u16) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ptr_u32(place: &str, rvalue: &str, constant: u32) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ptr_u64(place: &str, rvalue: &str, constant: u64) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ptr_u128(place: &str, rvalue: &str, constant: u128) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ptr_f32(place: &str, rvalue: &str, constant: f32) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ptr_f64(place: &str, rvalue: &str, constant: f64) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ptr_char(place: &str, rvalue: &str, constant: char) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ptr_bool(place: &str, rvalue: &str, constant: bool) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
//
//pub fn ptr_str(place: &str, rvalue: &str, constant: &str) {
//    let place: Place = place.try_into().unwrap();
//    println!("{place:?}");
//
//    let rvalue: Rvalue = rvalue.try_into().unwrap();
//    println!("{rvalue:?}");
//
//    println!("{constant:?}");
//}
