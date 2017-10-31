mod cpu;
mod mem;
mod monitor;
use cpu::Cpu;
use mem::Memory;
use monitor::Monitor;

fn main() {
    println!("Rusty8  v0.001");
    println!("MOS 6502/65C02/65CE02 Monitor and Emulator");
    println!("Copyright (c) 2017 Hernan Di Pietro");
    println!("Type LICENSE for license terms.");
    println!("");
    println!("CPU Mode 6502");
        
    let mut sys_mem = Memory::new();
    let mut sys_cpu = Cpu::new(&mut sys_mem);
    sys_cpu.reset();

    let mut mon = Monitor::new(&mut sys_cpu);
    mon.exec_command(&monitor::Command::Reg{ reg: String::new(), val: 0 });

    loop {
        let cmd = mon.do_prompt();
        if !mon.exec_command(&cmd) {
            break;
        }
    }  

    println!("BYE!");
}
