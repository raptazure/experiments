pub fn strings() {
    let mut _s = String::new();
    let _s1 = "initial contents".to_string();
    let _s2 = String::from("initial contents");
    let mut s = String::from("lo");
    s.push('l');

    let mut s1 = String::from("foo");
    let s2 = "bar";
    s1.push_str(s2);
    println!("s2 is {}", s2);

    let s3 = String::from("Hello, ");
    let s4 = String::from("world!");
    let s5 = s3 + &s4; // note s1 has been moved here and can no longer be used
                       // fn add(self, s: &str) -> String {
    println!("{}", s5);

    let s6 = String::from("tic");
    let s7 = String::from("tac");
    let s8 = String::from("toe");

    let s9 = s7 + "-" + &s8 + "-" + &s8;
    println!("{}", s9);

    let s7 = String::from("tac");
    let _s9 = format!("{}-{}-{}", s6, s7, s8);

    // Rust strings don’t support indexing.
    // A String is a wrapper over a Vec<u8>
    // An index into the string’s bytes will not always correlate to a valid Unicode scalar value.
    // Another point about UTF-8 is that there are actually three relevant ways to look at strings from Rust’s perspective: as bytes, scalar values, and grapheme clusters (the closest thing to what we would call letters).
    // A final reason Rust doesn’t allow us to index into a String to get a character is that indexing operations are expected to always take constant time (O(1)). But it isn’t possible to guarantee that performance with a String, because Rust would have to walk through the contents from the beginning to the index to determine how many valid characters there were.

    let hello = "Здравствуйте";
    println!("{}", &hello[0..4]);

    for c in "नमस्ते".chars() {
        println!("{}", c);
    }

    for b in "नमस्ते".bytes() {
        println!("{}", b);
    }
}
