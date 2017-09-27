mod mem;
mod cpu;

fn main() {
    let mut my_cpu = cpu::Cpu::new();
    my_cpu.exec();
    println!("Hello, world!");
}
