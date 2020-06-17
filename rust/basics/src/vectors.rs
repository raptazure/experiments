pub fn run() {
    let mut numbers: Vec<i32> = vec![1, 2, 3, 4, 5];
    numbers[2] = 20;
    println!("vector: {:?}", numbers);

    // Get single val
    println!("{}", numbers[0]);

    // Add on to the vector
    numbers.push(6);
    // Pop off last value
    numbers.pop();

    // Get length
    println!("Vector Length: {}", numbers.len());

    // Vectors are heap allocated
    println!("Occupies {} bytes", std::mem::size_of_val(&numbers));

    // Get slice
    let slice: &[i32] = &numbers[0..2];
    println!("Slice: {:?}", slice);

    // Loop through vector values
    for x in numbers.iter() {
        print!("{}", x);
    }

    // Loop & mutate values
    for x in numbers.iter_mut() {
        *x *= 2;
    }

    print!("{:?}", numbers);
}
