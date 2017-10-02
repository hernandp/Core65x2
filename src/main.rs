mod machine;
mod cpu;
mod mem;

fn main() {
    let mut machine = machine::Machine ::new();
    machine.boot();
    println!("Hello, world!");
}
