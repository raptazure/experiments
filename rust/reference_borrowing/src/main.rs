fn main() {
    let s1 = String::from("hello");
    let mut s2 = String::from("hello");
    let len = calculate_length(&s1);

    println!("The length of '{}' is {}", s1, len);

    // you can have only one mutable reference to a particular piece of data in a particular scope. (to prevent data race)
    change(&mut s2);
    ref_scope();
}

// We call having references as function parameters borrowing
// Just as variables are immutable by default, so are references. We’re not allowed to modify something we have a reference to.
fn calculate_length(s: &String) -> usize {
    // s is a reference to a String
    s.len()
} // Here, s goes out of scope. But because it does not have ownership of what
  // it refers to, nothing happens.

fn change(some_string: &mut String) {
    some_string.push_str(", world!");
}

// we can use curly brackets to create a new scope, allowing for multiple mutable references, just not simultaneous ones
// We also cannot have a mutable reference while we have an immutable one. Users of an immutable reference don’t expect the values to suddenly change out from under them! However, multiple immutable references are okay because no one who is just reading the data has the ability to affect anyone else’s reading of the data.

// a reference’s scope starts from where it is introduced and continues through the last time that reference is used
fn ref_scope() {
    let mut s = String::from("hello");

    let r1 = &s;
    let r2 = &s;
    println!("{} and {}", r1, r2);

    let r3 = &mut s;
    println!("{}", r3);
}

// the compiler guarantees that references will never be dangling references: if you have a reference to some data, the compiler will ensure that the data will not go out of scope before the reference to the data does. References must always be valid.
