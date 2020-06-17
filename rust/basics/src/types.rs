/*
Primitive Types--
integers: u8, i8, u16, i16, u32, i32, u64, i64, u128, i128 (number of bits they take in memory)
floats: f32, f64
boolean: (bool)
characters: (char)
tuples
arrays
*/

// Rust is a staticlly typed language, which means that it must know the types of all variables at compile time, however, the compiler can usually infer what type we want to use based on the value and how we use it.

pub fn run() {
    // Default is "i32"
    let x = 1;

    // Default is "f64"
    let y = 2.5;

    // Add explicit type
    let z: i64 = 2333333333;

    // Find max size
    println!("Max i32: {}", std::i32::MAX);
    println!("Max i64: {}", std::i64::MAX);

    // Boolean
    let is_active = true;

    // Get boolean from exprssion
    let is_greater: bool = 10 > 5;

    let face = '😍';
    println!("{:?}", (x, y, z, is_active, is_greater, face));
}
