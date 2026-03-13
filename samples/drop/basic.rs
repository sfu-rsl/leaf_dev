fn main() {
    let _obj = CustomDrop {
        data: String::from("custom object"),
    };
}

struct CustomDrop {
    data: String,
}

impl Drop for CustomDrop {
    fn drop(&mut self) {
        self.data = String::from("dropped");
    }
}
