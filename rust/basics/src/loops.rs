pub fn run() {
    let mut count = 0;

    // Infinite loop
    loop {
        count += 1;
        println!("Number: {}", count);

        if count == 5 {
            break;
        }
    }

    // While (FizzBuzz)
    while count <= 25 {
        if count % 15 == 0 {
            println!("FizzBuzz");
        } else if count % 5 == 0 {
            println!("Buzz");
        } else if count % 3 == 0 {
            println!("Fizz");
        } else {
            println!("{}", count);
        }

        count += 2;
    }
}
