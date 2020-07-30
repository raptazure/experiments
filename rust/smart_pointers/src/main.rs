mod box_p;
mod rc;
mod ref_cycles;

fn main() {
    println!("Hello, world!");
    box_p::run();
    rc::run();
    ref_cycles::cycle();
    ref_cycles::tree();
}
