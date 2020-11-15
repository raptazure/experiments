// Function Pointers
// Function pointers implement all three of the closure traits (Fn, FnMut, and FnOnce)
// It’s best to write functions using a generic type and one of the closure traits so your functions can accept either functions or closures.
// An example of where you would want to only accept fn and not closures is when interfacing with external code that doesn’t have closures: C functions can accept functions as arguments, but C doesn’t have closures.
fn add_one(x: i32) -> i32 {
    x + 1
}

fn do_twice(f: fn(i32) -> i32, arg: i32) -> i32 {
    f(arg) + f(arg)
}

enum Status {
    Value(u32),
    Stop,
}

pub fn run() {
    let answer = do_twice(add_one, 5);

    println!("The answer is: {}", answer);

    let list_of_numbers = vec![1, 2, 3];
    let _list_of_strings: Vec<String> = list_of_numbers.iter().map(|i| i.to_string()).collect();

    let _list_of_strings: Vec<String> = list_of_numbers.iter().map(ToString::to_string).collect();

    let _list_of_statuses: Vec<Status> = (0u32..20).map(Status::Value).collect();
}

// Returning Closures: use a trait object
fn returns_closure() -> Box<dyn Fn(i32) -> i32> {
    Box::new(|x| x + 1)
}
