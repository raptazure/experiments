let some_u8_value = Some(0u8);
match some_u8_value {
    Some(3) => println!("Three"),
    _ => (),
}

if let Some(3) = some_u8_value {
    println!("Three")
}

// you can think of if let as syntax sugar for a match that runs code when the value matches one pattern and then ignores all other values.