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

    pub fn read_byte(&self, addr: u16) -> u8 {
        self.mem_array[addr as usize]
    }

    pub fn write_byte(&mut self, addr: u16, v: u8) {
        self.mem_array[addr as usize] = v;
    }

}

