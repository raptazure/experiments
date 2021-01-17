use std::vec;

struct OneToTen(u32);

fn one_to_ten() -> OneToTen {
    OneToTen(1)
}

impl Iterator for OneToTen {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0 > 10 {
            None
        } else {
            let res = Some(self.0);
            self.0 += 1;
            res
        }
    }
}

struct Fibs {
    x: u32,
    y: u32,
}

fn fibs() -> Fibs {
    Fibs { x: 0, y: 1 }
}

impl Iterator for Fibs {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        let orig_x = self.x;
        let orig_y = self.y;

        match orig_x.checked_add(orig_y) {
            // overflow
            None => None,

            // no overflow
            Some(new_y) => {
                self.x = orig_y;
                self.y = new_y;

                Some(orig_x)
            }
        }
    }
}

enum Fibs2 {
    Done,
    OneLeft(u32),
    Running(u32, u32),
}

fn fibs2() -> Fibs2 {
    Fibs2::Running(0, 1)
}

impl Iterator for Fibs2 {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        use Fibs2::*;

        match *self {
            Done => None,
            OneLeft(x) => {
                *self = Done;
                Some(x)
            }
            Running(orig_x, orig_y) => {
                *self = match orig_x.checked_add(orig_y) {
                    // overflow
                    None => OneLeft(orig_y),
                    Some(new_y) => Running(orig_y, new_y),
                };

                Some(orig_y)
            }
        }
    }
}

struct Doubler<I> {
    iter: I,
}

impl<I> Iterator for Doubler<I>
where
    I: Iterator,
    I::Item: std::ops::Mul<Output = I::Item> + From<u8>,
{
    type Item = I::Item;
    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            None => None,
            Some(x) => Some(x * From::from(2u8)),
        }
    }
}

// fn sum<I>(iter: I) -> I::Item
// where
//     I: Iterator,
//     I::Item: std::ops::Add<Output = I::Item> + From<u8>,
// {
//     iter.fold(From::from(0u8), std::ops::Add::add)
// }

fn double(x: &mut u32) {
    *x *= 2;
}

struct InfiniteUnit;

// impl IntoIterator for InfiniteUnit {
//     type Item = ();
//     type IntoIter = std::iter::Repeat<()>;
//     fn into_iter(self) -> Self::IntoIter {
//         std::iter::repeat(())
//     }
// }

impl IntoIterator for InfiniteUnit {
    type Item = ();
    type IntoIter = InfiniteUnitIter;
    fn into_iter(self) -> Self::IntoIter {
        InfiniteUnitIter
    }
}

struct InfiniteUnitIter;

impl Iterator for InfiniteUnitIter {
    type Item = ();
    fn next(&mut self) -> Option<Self::Item> {
        Some(())
    }
}

fn main() {
    let mut x = 5;
    double(&mut x);
    println!("{}", x);

    for i in one_to_ten() {
        println!("{}", i);
    }

    for i in fibs().take(10) {
        println!("{}", i);
    }

    for i in fibs2().take(47) {
        println!("{}", i);
    }

    let orig_iter = 1..11;
    let double_iter = Doubler { iter: orig_iter };
    for i in double_iter {
        println!("{}", i);
    }

    for i in (1..11).map(|x| x * 2) {
        println!("{}", i);
    }

    for i in (1..11).skip(3).map(|x| x + 1).filter(|x| x % 2 == 0) {
        println!("{}", i);
    }

    let my_vec: Vec<u32> = (1..11).collect();
    println!("{:?}", my_vec);

    let res = (1..11).fold(0, std::ops::Add::add);
    println!("{}", res);

    let mut nums = vec![1, 2, 3, 4, 5];
    for i in 1..3 {
        // for j in nums.iter() {
        for j in &nums {
            println!("{},{}", i, j);
        }
    }

    for i in 1..3 {
        // for j in nums.iter_mut() {
        for j in &mut nums {
            println!("{},{}", i, j);
            *j *= 2;
        }
    }

    // Whenever you use for x in y, the compiler automatically calls into_iter() on y.
    // This allows you to loop over types which don’t actually have their own implementation of Iterator.
    for i in 1..3 {
        let nums = vec![1, 2, 3, 4, 5];
        for j in nums.into_iter() {
            println!("{},{}", i, j);
        }
    }

    let mut count = 0;
    for _ in InfiniteUnit {
        count += 1;
        println!("count == {}", count);
        if count >= 5 {
            break;
        }
    }
}
