use std::fmt;
use std::io::Error;

type Thunk = Box<dyn Fn() + Send + 'static>;

fn takes_long_type(f: Thunk) {
    // --snip--
}

fn returns_long_type() -> Thunk {
    // --snip--
}

// pub trait Write {
//     fn write(&mut self, buf: &[u8]) -> Result<usize, Error>;
//     fn flush(&mut self) -> Result<(), Error>;

//     fn write_all(&mut self, buf: &[u8]) -> Result<(), Error>;
//     fn write_fmt(&mut self, fmt: fmt::Arguments) -> Result<(), Error>;
// }

// type Result<T> = std::result::Result<T, std::io::Error>;

pub trait Write {
    fn write(&mut self, buf: &[u8]) -> Result<usize>;
    fn flush(&mut self) -> Result<()>;

    fn write_all(&mut self, buf: &[u8]) -> Result<()>;
    fn write_fmt(&mut self, fmt: Arguments) -> Result<()>;
}

// never type
// let guess: u32 = match guess.trim().parse() {
//     Ok(num) => num,
//     Err(_) => continue,
// };
// continue has a ! value. That is, when Rust computes the type of guess, it looks at both match arms, the former with a value of u32 and the latter with a ! value. Because ! can never have a value, Rust decides that the type of guess is u32.

impl<T> Option<T> {
    pub fn unwrap(self) -> T {
        match self {
            Some(val) => val,
            None => panic!("called `Option::unwrap()` on a `None` value"),
        }
    }
}
// Rust sees that val has the type T and panic! has the type !, so the result of the overall match expression is T. This code works because panic! doesn’t produce a value; it ends the program.

// DST - dynamically sized types
// str is a DST. We can’t know how long the string is until runtime, meaning we can’t create a variable of type str, nor can we take an argument of type str.
// So although a &T is a single value that stores the memory address of where the T is located, a &str is two values: the address of the str and its length. As such, we can know the size of a &str value at compile time: it’s twice the length of a usize. That is, we always know the size of a &str, no matter how long the string it refers to is. In general, this is the way in which dynamically sized types are used in Rust: they have an extra bit of metadata that stores the size of the dynamic information. The golden rule of dynamically sized types is that we must always put values of dynamically sized types behind a pointer of some kind.
// Every trait is a dynamically sized type we can refer to by using the name of the trait. To use traits as trait objects, we must put them behind a pointer, such as &dyn Trait or Box<dyn Trait> (Rc<dyn Trait> would work too).
// To work with DSTs, Rust has a particular trait called the Sized trait to determine whether or not a type’s size is known at compile time. This trait is automatically implemented for everything whose size is known at compile time. In addition, Rust implicitly adds a bound on Sized to every generic function.
fn generic<T: Sized>(t: T) {
    // --snip--
}
// A trait bound on ?Sized is the opposite of a trait bound on Sized: we would read this as “T may or may not be Sized.” This syntax is only available for Sized, not any other traits.
fn generic<T: ?Sized>(t: &T) {
    // --snip--
}
