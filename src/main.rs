mod cpu;
mod mem;

use cpu::Cpu;
use mem::Memory;

fn main() {
    println!("Rust-65XX02 Machine Emulator v0.001");

    let mut sys_mem = Memory::new();
    let mut sys_cpu = Cpu::new(&mut sys_mem);

    sys_cpu.exec();

}
