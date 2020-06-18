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

    println!("{} - {} - {} - {}", quotient, remainder, z, heart_eyed_cat);
    println!("{:?}", tup);
    println!("The value of y is: {}", y);
    println!("{} {} {}", five_hundred, six_point_four, one);
    println!("{:?}", [a, b].concat());
}
