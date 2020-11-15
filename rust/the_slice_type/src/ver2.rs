pub fn slice_param() {
    let my_string = String::from("hello wolrd");
    // first_word works on slices of `String`s
    let word1 = first_word(&my_string[..]);

    let my_string_literal = "hello world";

    // first_word works on slices of string literals
    let word2 = first_word(&my_string_literal[..]);

    // Because string literals *are* string slices already,
    // this works too, without the slice syntax!
    let word3 = first_word(my_string_literal);

    println!("{} {} {}", word1, word2, word3);
}

fn first_word(s: &str) -> &str {
    let bytes = s.as_bytes();
    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[..i];
        }
    }
    &s[..]
}
