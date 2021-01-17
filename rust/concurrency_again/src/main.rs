use std::sync::Arc;
use std::sync::Mutex;
use std::thread::{sleep, spawn};
use std::time::Duration;

fn main() {
    let msg = Arc::new(Mutex::new(String::from("fearless")));
    for _ in 1..11 {
        let msg = msg.clone();
        let inner = move || {
            let mut msg = msg.lock().unwrap();
            msg.push('!');
            println!("{}", msg);
        };
        spawn(inner);
        sleep(Duration::new(1, 0));
    }
}
