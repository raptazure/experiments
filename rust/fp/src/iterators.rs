pub trait Iterator {
    type Item;
    fn next(&mut self) -> Option<Self::Item>;
}

fn iterator_sum() {
    let v1 = vec![1, 3, 4];
    let v1_iter = v1.iter();

    let total: i32 = v1_iter.sum();
    assert_eq!(total, 8);
}

fn method_produce_iter() {
    // Calling the map method to create a new iterator
    // then calling the collect method to consume the new iterator and create a vector
    let v1: Vec<i32> = vec![1, 2, 3];
    let v2: Vec<_> = v1.iter().map(|x| x + 1).collect();

    assert_eq!(v2, vec![2, 3, 4]);
}

#[derive(Debug, PartialEq)]
struct Shoe {
    size: u32,
    style: String,
}

fn shoes_in_my_size(shoes: Vec<Shoe>, shoe_size: u32) -> Vec<Shoe> {
    shoes.into_iter().filter(|s| s.size == shoe_size).collect()
}

pub fn run() {
    iterator_sum();
    method_produce_iter();
    println!(
        "{:?}",
        shoes_in_my_size(
            vec![Shoe {
                size: 10,
                style: String::from("neek")
            }],
            10
        )
    );
}

#[test]
fn iterator_demonstration() {
    let v1 = vec![1, 2, 3];

    let mut v1_iter = v1.iter();

    assert_eq!(v1_iter.next(), Some(&1));
    assert_eq!(v1_iter.next(), Some(&2));
    assert_eq!(v1_iter.next(), Some(&3));
    assert_eq!(v1_iter.next(), None);
}

#[test]
fn filter_by_size() {
    let shoes = vec![
        Shoe {
            size: 10,
            style: String::from("sneaker"),
        },
        Shoe {
            size: 13,
            style: String::from("sandal"),
        },
        Shoe {
            size: 10,
            style: String::from("boot"),
        },
    ];

    let in_my_size = shoes_in_my_size(shoes, 10);

    assert_eq!(
        in_my_size,
        vec![
            Shoe {
                size: 10,
                style: String::from("sneaker")
            },
            Shoe {
                size: 10,
                style: String::from("boot")
            },
        ]
    );
}
