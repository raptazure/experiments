#[derive(Debug)]
struct Point<T, U> {
    x: T,
    y: U,
}

pub fn run() {
    let both_integer = Point { x: 5, y: 10 };
    let both_float = Point { x: 1.0, y: 4.0 };
    let integer_and_float = Point { x: 5, y: 4.0 };
    println!(
        "{:?}, {:?}, {:?}",
        both_float, both_integer, integer_and_float
    );
}

// enum Option<T> {
//     Some(T),
//     None,
// }

// enum Result<T, E> {
//     Ok(T),
//     Err(E),
// }
