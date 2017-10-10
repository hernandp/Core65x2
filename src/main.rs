mod cpu;
mod mem;
mod cmdint;
use cpu::Cpu;
use mem::Memory;

fn main() {
    println!("Rusty8  v0.001");
    println!("MOS 6502/65C02/65CE02 Monitor and Emulator");
    println!("Copyright (c) 2017 Hernan Di Pietro");
    println!("Type LICENSE for license terms.");
    
    let mut sys_mem = Memory::new();
    let mut sys_cpu = Cpu::new(&mut sys_mem);

    loop {
        let cmd = cmdint::do_prompt();
        if cmd == cmdint::Command::Quit {
            println!("BYE!");
            break;
        }
        if !cmdint::exec(&cmd, &mut sys_cpu) {
            break;
        }
    }  
}
