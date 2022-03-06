fn main() {
    let b = false;

    switch(b);
}

fn switch(b: bool) {
    match b {
        true => println!("true"),
        false => println!("false"),
    }
}
