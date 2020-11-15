// Another data type that does not have ownership is the slice. Slices let you reference a contiguous sequence of elements in a collection rather than the whole collection.
mod ver2;

fn main() {
    // let mut s = String::from("hello");
    let s = String::from("hello");
    let word = first_word(&s);
    // s.clear(); // error!
    println!("the first word is: {}", word);

    // The type of _s1 here is &str: it’s a slice pointing to that specific point of the binary. This is also why string literals are immutable; &str is an immutable reference.
    let _s1 = "Hello, world!"; // String Literals Are Slices

    ver2::slice_param();

    let a = [1, 2, 3, 4, 5];
    let slice = &a[1..3]; // &[i32] type
    println!("{:?}", slice);
}

// fn first_word(s: &String) -> usize {
//     let bytes = s.as_bytes();
//     for (i, &item) in bytes.iter().enumerate() {
//         if item == b' ' {
//             return i;
//         }
//     }
//     s.len()
// }

fn first_word(s: &String) -> &str {
    let bytes = s.as_bytes();
    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[..i];
        }
    }
    &s[..]
}
