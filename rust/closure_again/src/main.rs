fn call_with_hi<F>(f: F)
where
    F: Fn(&str) -> (),
{
    f("Hi");
}

fn call_with_bye<F>(f: F)
where
    F: Fn(&str),
{
    f("Bye");
}

fn call_five_times<F>(mut f: F)
where
    // for functions which do mutate their environment
    F: FnMut(),
{
    for _ in 1..6 {
        f();
    }
}

fn call_once<F>(f: F)
where
    F: FnOnce(),
{
    f();
}

fn main() {
    let name = String::from("Alice");
    let say_something = |msg: &str| println!("{}, {}", msg, name);
    call_with_hi(say_something);
    call_with_bye(say_something);

    let mut count = 0;
    let visit = || {
        count += 1;
        println!("You are visitor #{}", count);
    };

    call_five_times(visit);

    // Every value which is a Fn is automatically an FnMut
    call_five_times(|| println!("hello world"));

    let welcome = || {
        let mut name1 = name;
        name1 += "and Flandre";
        println!("Welcome, {}", name1);
    };

    // further function subtyping: every Fn and every FnMut are also FnOnces
    // because any context you can guarantee the function will only be called
    // once is safe for running functions with mutable or immutable environments.
    call_once(welcome);

    // closures can own data, functions cannot

    // I’d like the closure to own the values it captures, but I don’t want
    // to have to force a use by value to do it. That will allow a closure to
    // outlive the original scope of the value, but still allow a closure to
    // be called multiple times. And to do that, we introduce `move`.
    let say_hi_scope = {
        // owned by the smaller scope
        let name_outer = String::from("Alice");

        // The ownership of name_outer passes from the original scope to the
        // closure itself. We still only use it by reference, and therefore
        // we can call it multiple times.
        move || {
            let name_inner = &name_outer;
            println!("Hello, {}", name_inner);
        }
    };

    // syntactically invalid, name_outer isn't in this scope
    //println!("Using name from main: {}", name_outer); // error!

    say_hi_scope();
    say_hi_scope();

    test();
    ex();
}

// • Within a closure, a variable can be used by value, mutable reference, or immutable reference
// • In addition, all variables captured by a closure can be captured by value, by mutable reference, or by immutable reference
// • We cannot use a variable in a stronger way than it was captured. If it was captured by mutable reference, it can be used by immutable reference, but not by value.
// • To solve lifetime issues, we can force a closure to capture by value with the move keyword.
// • Short of the move keyword, Rust will be reluctant, and capture in the weakest way allowed by the body of the closure.
// • Regarding the traits of closures:
// ◦ If a closure uses anything by value, then the closure is a FnOnce
// ◦ Otherwise, if a closure uses anything by mutable reference, then the closure is a FnMut, which automatically implies FnOnce as well
// ◦ Otherwise, a closure is a Fn, which automatically implies both FnMut and FnOnce

fn call_fn_mut<F>(mut f: F)
where
    F: FnMut(),
{
    f()
}

fn call_fn_once<F>(f: F)
where
    F: FnOnce(),
{
    f()
}

fn test() {
    let mut say_hi = {
        let mut name = String::from("Alice");
        move || {
            name += " and Bob";
            println!("Hello, {}", name);
        }
    };
    //call_fn(say_hi);
    call_fn_mut(&mut say_hi);
    call_fn_once(&mut say_hi);

    let mut name = String::from("David");
    let say_hey = || {
        println!("Hello, {}", name); // use by ref
        name += " and Bob"; // use by mut ref
        std::mem::drop(name); // use by value
    };

    call_fn_once(say_hey);
}

// When receiving functions as arguments, the most lenient thing to start with is a FnOnce.

fn ex() {
    let nums: Vec<u32> = (1..11).collect();

    for _ in 1..3 {
        for i in nums.iter().map(|x| x * 2) {
            println!("{}", i);
        }
    }
}
