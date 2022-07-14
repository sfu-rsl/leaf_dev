const NAME: &str = "ABC";

fn get_name(name: &mut String) -> Option<()> {
    *name = String::from(NAME);
    Some(())
}

fn main() {
    let mut name = String::new();
    println!("What's your name?");
    match get_name(&mut name) {
        Some(_) => match name.trim_end() {
            "root" => println!("What is your command?"),
            name => println!("Hello, {}!", name),
        },
        None => {}
    }
}
