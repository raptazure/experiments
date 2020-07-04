use std::collections::HashMap;

pub fn hashmap() {
    let mut scores = HashMap::new();
    scores.insert(String::from("blue"), 10);

    let teams = vec![String::from("red"), String::from("orange")];
    let initial_scores = vec![10, 50];

    let mut scores1: HashMap<_, _> = teams.into_iter().zip(initial_scores.into_iter()).collect();
    scores1.insert(String::from("dark"), 30);
    println!("{:?}", scores1);
    let team_name = String::from("dark");
    println!("{:?}", scores1.get(&team_name));

    scores.insert(String::from("blue"), 25);
    match scores.get(&String::from("blue")) {
        Some(25) => println!("{}!", 25),
        None => println!("Not here."),
        _ => println!("Not 25."),
    }

    for (key, value) in &scores1 {
        println!("{}: {}", key, value);
    }

    scores.entry(String::from("yellow")).or_insert(50);
    scores.entry(String::from("blue")).or_insert(50);
    println!("{:?}", scores);

    let text = "hello world wonderful world";
    let mut map = HashMap::new();
    for word in text.split_whitespace() {
        let count = map.entry(word).or_insert(0);
        *count += 1;
    }

    println!("{:?}", map);
}
