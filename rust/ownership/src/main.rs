mod ownership_functions;
mod return_values;

fn main() {
    let mut s = String::from("hello");
    s.push_str(", world!");
    println!("{}", s);

    // When we assign s1 to s2, the String data is copied, meaning we copy the pointer, the length, and the capacity that are on the stack.
    // We do not copy the data on the heap that the pointer refers to
    // Rust also invalidates the first variable
    // instead of being called a shallow copy, it’s known as a move
    let s1 = String::from("hello");
    // s1 is moved to s2
    let s2 = s1;
    // Instead of trying to copy the allocated memory
    // Rust considers s1 to no longer be valid
    // Rust doesn’t need to free anything when s1 goes out of scope.
    println!("{}", s2);

    // Rust will never automatically create “deep” copies of your data. Therefore, any automatic copying can be assumed to be inexpensive in terms of runtime performance.

    // If we do want to deeply copy the heap data of the String, not just the stack data, we can use a common method called clone
    let s3 = String::from("hello");
    let s4 = s3.clone();
    println!("s3 = {}, s4 = {}", s3, s4);

    // Copy trait: any group of simple scalar values can be Copy, and nothing that requires allocation or is some form of resource is Copy.
    // Types that are Copy: integer, Boolean, floating point, character, tuples (only contain types that are also Copy)
    let x = 5;
    let y = x;
    println!("x = {}, y = {}", x, y);

    // Passing a variable to a function will move or copy, just as assignment does.
    ownership_functions::ownership_func();
    return_values::return_values_and_scope();
}
