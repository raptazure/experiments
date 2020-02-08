// Primitive str - immutable fixed-length string somewhere in memory
// String - Growable, heap-allocated data structure - use when you need to modify or own string data

pub fn run() {
    let mut hello = String::from("Hello");

    // Get length
    println!("length: {}", hello.len());

    // Push char
    hello.push('W');

    // Capacity in bytes
    println!("Capacity: {}", hello.capacity());

    // Check if empty
    println!("Is empty: {}", hello.is_empty());
    // Push string
    hello.push_str("orld");

    // Contains
    println!("Contains 'world': {}", hello.contains("world"));

    // Replace
    println!("Replace: {}", hello.replace("World", "There"));

    // Loop through string by whitespace
    for word in hello.split_whitespace() {
        println!("{}", word);
    }

    // Create string with capacity
    let mut s = String::with_capacity(10);
    s.push('a');
    s.push('b');

    // Assertion testing
    assert_eq!(2, s.len());
    assert_eq!(10, s.capacity());

    println!("{}", s);

    println!("{}", hello);
}
