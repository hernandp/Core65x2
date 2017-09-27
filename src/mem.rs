// Static size: this is a 64K-address space machine anyway
//
const MACHINE_MEMORY_SIZE: usize = 64 * 1024;

pub struct Memory {
    mem_array: [u8; MACHINE_MEMORY_SIZE]
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
            mem_array: [0; MACHINE_MEMORY_SIZE]
        }
    }

    pub fn read_byte(&self, addr: usize) -> u8 {
        self.mem_array[addr]
    }

    pub fn write_byte(&mut self, addr: usize, v: u8) {
        self.mem_array[addr] = v;
    }

}

