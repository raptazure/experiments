// Variables hold primitive data or references to data
// Variables are immutable by default
// Rust is a block-scoped language

pub fn run() {
    let name = "meow";
    let mut age = 10;
    println!("My name is {} and I am {}", name, age);
    age += 1;
    println!("My name is {} and I am {}", name, age);

    // Define constant
    const ID: i32 = 001;
    println!("ID: {}", ID);

    // Assisn multiple vars
    let (my_name, my_age) = ("meow", 12);
    println!("{} is {}", my_name, my_age);
}
