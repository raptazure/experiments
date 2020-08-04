mod blog;
mod gui;
use blog::Post;
mod blog_v2;
use blog_v2::Post as PostV2;
use gui::{Button, Draw, Screen};

struct SelectBox {
    width: u32,
    height: u32,
    options: Vec<String>,
}

impl Draw for SelectBox {
    fn draw(&self) {
        println!("{} {} {:?}", self.width, self.height, self.options);
        // draw a select box
    }
}

fn main() {
    let screen = Screen {
        components: vec![
            Box::new(SelectBox {
                width: 75,
                height: 10,
                options: vec![
                    String::from("Yes"),
                    String::from("Maybe"),
                    String::from("No"),
                ],
            }),
            Box::new(Button {
                width: 50,
                height: 10,
                label: String::from("OK"),
            }),
        ],
    };

    screen.run();

    let mut post = Post::new();

    post.add_text("I ate a salad for lunch today");
    assert_eq!("", post.content());

    post.request_review();
    assert_eq!("", post.content());

    post.approve();
    assert_eq!("I ate a salad for lunch today", post.content());

    let mut post_v2 = PostV2::new();

    post_v2.add_text("I ate a salad for lunch today");

    let post_v2 = post_v2.request_review();

    let post_v2 = post_v2.approve();

    assert_eq!("I ate a salad for lunch today", post_v2.content());
}
