use std::ops::Deref;

#[derive(Debug)]
struct Person {
    name: Option<String>,
    age: Option<u32>,
}

fn print_person(mut person: Person) {
    match person.name {
        Some(ref name) => println!("Name is {}", name),
        None => println!("No name provided"),
    }
    match person.age {
        Some(ref mut age) => {
            println!("Age is {}", age);
            *age += 1;
        }
        None => println!("No age provided"),
    }
    println!("Full person value: {:?}", person);
}

struct Single<T> {
    next: Option<T>,
}

fn single<T>(t: T) -> Single<T> {
    Single { next: Some(t) }
}

impl<T> Iterator for Single<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        self.next.take()
    }
}

struct OrdinaryPerson {
    name: String,
    age: u32,
}

fn get_older_name<'a>(person1: &'a OrdinaryPerson, person2: &'a OrdinaryPerson) -> &'a String {
    if person1.age >= person2.age {
        &person1.name
    } else {
        &person2.name
    }
}

fn message_and_return<'a, 'b>(msg: &'a String, ret: &'b String) -> &'b String {
    println!("printing the msg: {}", msg);
    ret
}

fn foo(name: &String) -> &String {
    let msg = String::from("This is the message");
    message_and_return(&msg, &name)
}

// Every single string literal is a ref with the lifetime of `'static`
// fn name() -> &'static str {
//     "alice"
// }

// a new struct which can be borrowed into a slice
struct MyArray([u32; 5]);

impl MyArray {
    fn new() -> MyArray {
        MyArray([9; 5])
    }
}

impl Deref for MyArray {
    type Target = [u32];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// try to avoid lifatime parameters in data structures
// and use owned versions of values instead.
// struct LifetimePerson<'a> {
//     name: &'a str,
//     age: u32,
// }

fn main() {
    print_person(Person {
        name: Some(String::from("Alice")),
        age: Some(30),
    });

    let actual: Vec<u32> = single(42).collect();
    assert_eq!(vec![42], actual);

    let alice = OrdinaryPerson {
        name: String::from("Alice"),
        age: 30,
    };
    let bob = OrdinaryPerson {
        name: String::from("Bob"),
        age: 35,
    };
    let name = get_older_name(&alice, &bob);
    println!("Older person: {}", name);

    let name = String::from("Alice");
    let ret = foo(&name);
    println!("Return value: {}", ret);

    let ma = MyArray::new();
    let _: &MyArray = &ma;
    let _: &[u32] = &ma;

    // The data is stored in the program executable itself, and therefore
    // cannot be modified (thus always receiving an immutable reference)
    let bytearray: &'static [u8; 2] = b"hi";
    println!("{:?}", bytearray);

    // a string slice (&str) is essentially a newtype wrapper around
    // a byte slice (&[u8]), which is guaranteed to be in UTF-8 encoding.
    for arg in std::env::args() {
        println!(
            "arg: {}, characters: {}, bytes: {}",
            arg,
            arg.chars().count(),
            arg.bytes().count()
        );
    }
}
