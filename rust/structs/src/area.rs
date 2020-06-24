#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

pub fn area() {
    let rect1 = Rectangle {
        width: 30,
        height: 50,
    };

    println!("rect1 is {:#?}", rect1);
    println!(
        "The area of the rectangle is {} square pixels.",
        calc(&rect1)
    );
}

// borrow the struct rather than take ownership of it
fn calc(rect: &Rectangle) -> u32 {
    rect.width * rect.height
}
