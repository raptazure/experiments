enum List {
    Cons(i32, Box<List>),
    Nil,
}

use std::ops::Deref;
struct MyBox<T>(T);

impl<T> MyBox<T> {
    fn new(x: T) -> MyBox<T> {
        MyBox(x)
    }
}

impl<T> Deref for MyBox<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

use List::{Cons, Nil};

fn hello(name: &str) {
    println!("Hello, {}!", name);
}

struct CustomSmartPointer {
    data: String,
}

impl Drop for CustomSmartPointer {
    fn drop(&mut self) {
        println!("Dropping CustomSmartPointer with data: {}", self.data);
    }
}

pub fn run() {
    let _list = Cons(1, Box::new(Cons(2, Box::new(Cons(3, Box::new(Nil))))));
    let x = 5;
    let y = MyBox::new(x);

    //deref coercions
    let m = MyBox::new(String::from("Rust"));
    hello(&m);
    hello(&(*m)[..]);

    assert_eq!(5, x);
    assert_eq!(5, *y);

    let c = CustomSmartPointer {
        data: String::from("my stuff"),
    };
    let _d = CustomSmartPointer {
        data: String::from("other stuff"),
    };
    println!("CustomSmartPointer created.");
    // std::mem::drop
    drop(c);
    println!("CustomSmartPointer dropped before the end of main.");
}
