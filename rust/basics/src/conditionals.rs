pub fn run() {
    let age: u8 = 18;
    let check_id: bool = false;
    let knows_person_of_age = true;

    // if-else
    if age >= 21 || knows_person_of_age {
        println!("What do you want to drink?");
    } else if age < 21 && check_id {
        println!("Sorry.");
    } else {
        println!("Need to see your ID.")
    }

    // Shorthand if
    let is_of_age = if age >= 21 { true } else { false };
    println!("{}", is_of_age)
}
