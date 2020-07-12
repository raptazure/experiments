mod pair;

pub trait Summary {
    fn summarize(&self) -> String;
    fn summarize_author(&self) -> String;
    fn read_more(&self) -> String {
        format!("Read more from {}...", self.summarize_author())
    }
}

pub struct NewsArticle {
    pub headline: String,
    pub location: String,
    pub author: String,
    pub content: String,
}

impl Summary for NewsArticle {
    fn summarize(&self) -> String {
        format!("{}, by {} ({})", self.headline, self.author, self.location)
    }

    fn summarize_author(&self) -> String {
        format!("@{}", self.author)
    }
}

pub struct Tweet {
    pub username: String,
    pub content: String,
    pub reply: bool,
    pub retweet: bool,
}

impl Summary for Tweet {
    fn summarize(&self) -> String {
        format!("{}: {}", self.username, self.content)
    }

    fn summarize_author(&self) -> String {
        format!("@{}", self.username)
    }
}

// pub fn notify(item: impl Summary) {
//     println!("Breaking news! {}", item.summarize());
// }

pub fn notify<T: Summary>(item: T) {
    println!("Breaking news! {}", item.summarize());
}

// Trait Bound
// pub fn notify(item1: impl Summary, item2: impl Summary) {
// pub fn notify<T: Summary>(item1: T, item2: T) {

// pub fn notify(item: impl Summary + Display) {
// pub fn notify<T: Summary + Display>(item: T) {

// fn some_function<T: Display + Clone, U: Clone + Debug>(t: T, u: U) -> i32 {
// fn some_function<T, U>(t: T, u: U) -> i32
//     where T: Display + Clone,
//           U: Clone + Debug
// {

fn returns_summarizable() -> impl Summary {
    Tweet {
        username: String::from("horse_ebooks"),
        content: String::from("of course, as you probably already know, people"),
        reply: false,
        retweet: false,
    }
}

// blanket implementations
// impl<T: Display> ToString for T {
//     // --snip--
// }

fn main() {
    let tweet = returns_summarizable();

    println!("Check it: {}", tweet.read_more());
    notify(tweet);

    pair::run();
}
