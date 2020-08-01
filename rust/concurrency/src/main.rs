mod message_passing;
mod share_state;
mod threads;

fn main() {
    threads::run();
    message_passing::run();
    share_state::run();
}
