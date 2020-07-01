pub fn vectors() {
    // let v: Vec<i32> = Vec::new();
    let v = vec![1, 2, 3];
    let mut v1 = Vec::new();

    v1.push(5);
    v1.push(6);

    println!("{:?} {:?}", v, v1);

    let third: &i32 = &v[2];
    println!("the third element is {}", third);

    match v.get(1) {
        Some(second) => println!("The second element is {}", second),
        None => println!("there is no second element"),
    }

    let mut v2 = vec![100, 32, 57];
    for i in &v2 {
        println!("{}", i);
    }

    for i in &mut v2 {
        *i += 50;
        println!("{}", i);
    }

    enum SpreadsheetCell {
        Int(i32),
        Float(f64),
        Text(String),
    }

    let _row = vec![
        SpreadsheetCell::Int(3),
        SpreadsheetCell::Text(String::from("blue")),
        SpreadsheetCell::Float(10.12),
    ];
} // Like any other struct, a vector is freed when it goes out of scope
