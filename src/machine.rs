// Our machine constants
//
use mem::Memory;
use cpu::Cpu;

pub struct Machine<'a> {
    ram: Memory,
    cpu: Cpu<'a>,
    // MMU?
    // Sound?
    // VIC?
}

impl<'a> Machine<'a> {
    pub fn new() -> Machine<'a> {
        Machine {
            ram: Memory::new(),
            cpu: Cpu::new()
        }
    }

    pub fn boot(&mut self) {
        self.cpu.exec();
    }
}
