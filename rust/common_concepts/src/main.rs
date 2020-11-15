fn plus_one(x: i32) -> i32 {
    x + 1
}

fn main() {
    let quotient = 56.7 / 32.2;
    let remainder = 43 % 5;
    let z = 'ℤ';
    let heart_eyed_cat = '😻';
    let tup: (i32, f64, u8) = (500, 6.4, 1);
    let (_x, y, _z) = tup;
    let five_hundred = tup.0;
    let six_point_four = tup.1;
    let one = tup.2;
    let a: [i32; 5] = [1, 2, 3, 4, 5];
    let b = [3; 5];
    let x = 5;
    let x_1 = {
        let x = 3;
        // an expression - no ending semicolons (not a statement), return a value
        x + 1
    };
    let six = plus_one(5);

    println!("{} - {} - {} - {}", quotient, remainder, z, heart_eyed_cat);
    println!("{:?}", tup);
    println!("The value of y is: {}", y);
    println!("{} {} {}", five_hundred, six_point_four, one);
    println!("{:?}", [a, b].concat());
    println!("{} {}", x, x_1);
    println!("{}", six);

    let number = 3;
    if number % 3 != 1 {
        println!("{}", plus_one(number));
    } else {
        println!("else...");
    }
    // if-else variables must have a single type
    println!("The value is: {}", if true { 5 } else { 6 });

    let mut counter = 0;
    let result = loop {
        counter += 1;
        if counter == 10 {
            break counter * 2;
        }
    };

    println!("The result is {}", result);

    let mut number_1 = 3;
    while number_1 != 0 {
        println!("{}!", number_1);
        number_1 -= 1;
    }

    println!("LIFTOFF!!!");

    let arr = [10, 20, 30, 40, 50];
    for element in arr.iter() {
        println!("The value is: {}", element);
    }

    for number in (1..4).rev() {
        println!("{}!", number);
    }
    println!("LIFTOFF!!!");
}
