const MEM_SIZE: usize = 64 * 1024;
const BOOT_PC_ADDR: u16 = 0xFFFE;
const STACK_ADDR_BASE: u16 = 0x01FF;
const STACK_ADDR_LIMIT: u16 = 0x0100;

//
// Processor Status Register flags
//
const FLAG_CARRY: u8 = 0b0000_0001;
const FLAG_ZERO: u8 = 0b0000_0010;
const FLAG_INTR: u8 = 0b0000_0100;
const FLAG_DEC: u8 = 0b0000_1000;
const FLAG_BRK: u8 = 0b0001_0000;
const FLAG_OF: u8 = 0b0100_0000;
const FLAG_SIGN: u8 = 0b1000_0000;

//
// Addressing mode modifier for opcodes
//
enum AddrMode {
    Impl,
    Imm, // #$00
    ZP, // $00
    ZPX, // $00,X
    ZPY, // $00,Y
    ZPIndX, // ($00,X)
    ZPIndY, // ($00),Y
    Abs, // $0000
    AbsX, // $0000,X
    AbsY, // $0000,Y
    Ind, // ($0000)
    Rel, // PC + $00
}

// Register modifier for opcodes
enum RegMod {
    A,
    X,
    Y,
    SP,
    SR,
}

// 6502 CPU registers
struct Regs {
    A: u8,
    X: u8,
    Y: u8,
    PC: u16,
    SP: u8,
    SR: u8,
}

pub struct Cpu {
    regs: Regs,
    mem: [u8; MEM_SIZE],
}

impl Cpu {
    //
    // Initialize processor
    //
    pub fn new() -> Cpu {
        Cpu {
            regs: Regs {
                A: 0,
                X: 0,
                Y: 0,
                SP: 0,
                PC: BOOT_PC_ADDR,
                SR: 0,
            },
            mem: [0; MEM_SIZE],
        }
    }

    //
    // Execute instruction at current program counter.
    // Returns elapsed clock cycles.
    //
    pub fn exec(&mut self) -> u32 {
        match self.regs.PC {
            /* Move instructions */

            // LDA
            0xA9 => self.op_load(AddrMode::Imm, RegMod::A),
            0xA5 => self.op_load(AddrMode::ZP, RegMod::A),
            0xB5 => self.op_load(AddrMode::ZPX, RegMod::A),
            0xA1 => self.op_load(AddrMode::ZPIndX, RegMod::A),
            0xB1 => self.op_load(AddrMode::ZPIndY, RegMod::A),
            0xAD => self.op_load(AddrMode::Abs, RegMod::A),
            0xBD => self.op_load(AddrMode::AbsX, RegMod::A),
            0xB9 => self.op_load(AddrMode::AbsY, RegMod::A),

            // LDX
            0xA2 => self.op_load(AddrMode::Imm, RegMod::X),
            0xA6 => self.op_load(AddrMode::ZP, RegMod::X),
            0xB6 => self.op_load(AddrMode::ZPY, RegMod::X),
            0xAE => self.op_load(AddrMode::Abs, RegMod::X),
            0xBE => self.op_load(AddrMode::AbsY, RegMod::X),

            // LDY
            0xA0 => self.op_load(AddrMode::Imm, RegMod::Y),
            0xA4 => self.op_load(AddrMode::ZP, RegMod::Y),
            0xB4 => self.op_load(AddrMode::ZPX, RegMod::Y),
            0xAC => self.op_load(AddrMode::Abs, RegMod::Y),
            0xBC => self.op_load(AddrMode::AbsX, RegMod::Y),

            // STA
            0x85 => self.op_store(AddrMode::ZP, RegMod::A),
            0x95 => self.op_store(AddrMode::ZPX, RegMod::A),
            0x81 => self.op_store(AddrMode::ZPIndX, RegMod::A),
            0x91 => self.op_store(AddrMode::ZPIndY, RegMod::A),
            0x8D => self.op_store(AddrMode::Abs, RegMod::A),
            0x9D => self.op_store(AddrMode::AbsX, RegMod::A),
            0x99 => self.op_store(AddrMode::AbsY, RegMod::A),

            // STX
            0x86 => self.op_store(AddrMode::ZP, RegMod::X),
            0x96 => self.op_store(AddrMode::ZPY, RegMod::X),
            0x8E => self.op_store(AddrMode::Abs, RegMod::X),

            // STY
            0x84 => self.op_store(AddrMode::ZP, RegMod::X),
            0x94 => self.op_store(AddrMode::ZPX, RegMod::X),
            0x8C => self.op_store(AddrMode::Abs, RegMod::X),

            // TAX, TXA, TAY, TYA, TSX, TXS
            0xAA => self.op_tx(RegMod::A, RegMod::X),
            0x8A => self.op_tx(RegMod::X, RegMod::A),
            0xA8 => self.op_tx(RegMod::A, RegMod::Y),
            0x98 => self.op_tx(RegMod::Y, RegMod::A),
            0xBA => self.op_tx(RegMod::SP, RegMod::X),
            0x9A => self.op_tx(RegMod::X, RegMod::SP),

            // PLA / PHA / PLP / PHP (stack)
            0x68 => self.op_pull(RegMod::A),
            0x48 => self.op_push(RegMod::A),
            0x28 => self.op_pull(RegMod::SR),
            0x08 => self.op_push(RegMod::SR),

            /* Logical / Arithmetic */

            // ORA
            0x09 => self.op_or(AddrMode::Imm),
            0x05 => self.op_or(AddrMode::ZP),
            0x15 => self.op_or(AddrMode::ZPX),
            0x01 => self.op_or(AddrMode::ZPIndX),
            0x11 => self.op_or(AddrMode::ZPIndY),
            0x0D => self.op_or(AddrMode::Abs),
            0x1D => self.op_or(AddrMode::AbsX),
            0x19 => self.op_or(AddrMode::AbsY),

            // AND
            0x29 => self.op_and(AddrMode::Imm),
            0x25 => self.op_and(AddrMode::ZP),
            0x35 => self.op_and(AddrMode::ZPX),
            0x21 => self.op_and(AddrMode::ZPIndX),
            0x31 => self.op_and(AddrMode::ZPIndY),
            0x2D => self.op_and(AddrMode::Abs),
            0x3D => self.op_and(AddrMode::AbsX),
            0x39 => self.op_and(AddrMode::AbsY),

            // EOR
            0x49 => self.op_xor(AddrMode::Imm),
            0x45 => self.op_xor(AddrMode::ZP),
            0x55 => self.op_xor(AddrMode::ZPX),
            0x41 => self.op_xor(AddrMode::ZPIndX),
            0x51 => self.op_xor(AddrMode::ZPIndY),
            0x4D => self.op_xor(AddrMode::Abs),
            0x5D => self.op_xor(AddrMode::AbsX),
            0x59 => self.op_xor(AddrMode::AbsY),

            // ADC
            0x69 => self.op_adc(AddrMode::Imm),
            0x65 => self.op_adc(AddrMode::ZP),
            0x75 => self.op_adc(AddrMode::ZPX),
            0x61 => self.op_adc(AddrMode::ZPIndX),
            0x71 => self.op_adc(AddrMode::ZPIndY),
            0x6D => self.op_adc(AddrMode::Abs),
            0x7D => self.op_adc(AddrMode::AbsX),
            0x79 => self.op_adc(AddrMode::AbsY),

            // SBC
            0xE9 => self.op_sbc(AddrMode::Imm),
            0xE5 => self.op_sbc(AddrMode::ZP),
            0xF5 => self.op_sbc(AddrMode::ZPX),
            0xE1 => self.op_sbc(AddrMode::ZPIndX),
            0xF1 => self.op_sbc(AddrMode::ZPIndY),
            0xED => self.op_sbc(AddrMode::Abs),
            0xFD => self.op_sbc(AddrMode::AbsX),
            0xF9 => self.op_sbc(AddrMode::AbsY),

            // CMP
            0xC9 => self.op_cmp(AddrMode::Imm, RegMod::A),
            0xC5 => self.op_cmp(AddrMode::ZP, RegMod::A),
            0xD5 => self.op_cmp(AddrMode::ZPX, RegMod::A),
            0xC1 => self.op_cmp(AddrMode::ZPIndX, RegMod::A),
            0xD1 => self.op_cmp(AddrMode::ZPIndY, RegMod::A),
            0xCD => self.op_cmp(AddrMode::Abs, RegMod::A),
            0xDD => self.op_cmp(AddrMode::AbsX, RegMod::A),
            0xD9 => self.op_cmp(AddrMode::AbsY, RegMod::A),

            // CPX / CPY
            0xE0 => self.op_cmp(AddrMode::Imm, RegMod::X),
            0xE4 => self.op_cmp(AddrMode::ZP, RegMod::X),
            0xEC => self.op_cmp(AddrMode::Abs, RegMod::X),
            0xC0 => self.op_cmp(AddrMode::Imm, RegMod::Y),
            0xC4 => self.op_cmp(AddrMode::ZP, RegMod::Y),
            0xCC => self.op_cmp(AddrMode::Abs, RegMod::Y),

            // DEC
            0xC6 => self.op_dec(AddrMode::ZP, RegMod::A),
            0xD6 => self.op_dec(AddrMode::ZPX, RegMod::A),
            0xCE => self.op_dec(AddrMode::Abs, RegMod::A),
            0xDE => self.op_dec(AddrMode::AbsX, RegMod::A),

            // DEX/DEY
            0xCA => self.op_dec(AddrMode::Impl, RegMod::X),
            0x88 => self.op_dec(AddrMode::Impl, RegMod::Y),            

            // INC
            0xE6 => self.op_inc(AddrMode::ZP, RegMod::A),
            0xF6 => self.op_inc(AddrMode::ZPX, RegMod::A),
            0xEE => self.op_inc(AddrMode::Abs, RegMod::A),
            0xFE => self.op_inc(AddrMode::AbsX, RegMod::A),

            // INX/INY
            0xE8 => self.op_inc(AddrMode::Impl, RegMod::X),
            0xC8 => self.op_inc(AddrMode::Impl, RegMod::Y),            

            // ASL 
            0x0A => self.op_shift(AddrMode::Impl, true),
            0x06 => self.op_shift(AddrMode::ZP, true),
            0x16 => self.op_shift(AddrMode::ZPX, true),
            0x0E => self.op_shift(AddrMode::Abs, true),
            0x1E => self.op_shift(AddrMode::AbsX, true),
            
            // ROL
            0x2A => self.op_rot(AddrMode::Impl, true),
            0x26 => self.op_rot(AddrMode::ZP, true),
            0x36 => self.op_rot(AddrMode::ZPX, true),
            0x2E => self.op_rot(AddrMode::Abs, true),
            0x3E => self.op_rot(AddrMode::AbsX, true),

            // LSR
            0x4A => self.op_shift(AddrMode::Impl, false),
            0x46 => self.op_shift(AddrMode::ZP, false),
            0x56 => self.op_shift(AddrMode::ZPX, false),
            0x4E => self.op_shift(AddrMode::Abs, false),
            0x5E => self.op_shift(AddrMode::AbsX, false),            

            // ROR
            0x6A => self.op_rot(AddrMode::Impl, false),
            0x66 => self.op_rot(AddrMode::ZP, false),
            0x76 => self.op_rot(AddrMode::ZPX, false),
            0x6E => self.op_rot(AddrMode::Abs, false),
            0x7E => self.op_rot(AddrMode::AbsX, false),

            /* Jump and program-counter change instructions */

            // BPL / BMI / BVC / BVS / BCC / BCS / BNE / BEQ
            0x10 => self.op_jump(FLAG_SIGN, false),
            0x30 => self.op_jump(FLAG_SIGN, true),
            0x50 => self.op_jump(FLAG_OF, false),
            0x70 => self.op_jump(FLAG_OF, true),
            0x90 => self.op_jump(FLAG_CARRY, false),
            0xB0 => self.op_jump(FLAG_CARRY, true),
            0xD0 => self.op_jump(FLAG_ZERO, false),
            0xF0 => self.op_jump(FLAG_ZERO, true),

            // BRK/RTI
            0x00 => self.op_brk(),
            0x40 => self.op_rti(),

            // JSR/RTS
            0x20 => self.op_jsr(),
            0x60 => self.op_rts(),

            // JMP
            0x4C => self.op_jmp(AddrMode::Abs),
            0x6C => self.op_jmp(AddrMode::Ind),

            // BIT
            0x24 => self.op_bit(AddrMode::ZP),
            0x2C => self.op_bit(AddrMode::Abs),

            /* Flag instructions */

            // SEC/CLD/SED/CLI/SEI/CLV
            0x18 => self.op_setfl(FLAG_CARRY, false),
            0x38 => self.op_setfl(FLAG_CARRY, true),
            0xD8 => self.op_setfl(FLAG_DEC, false),
            0xF8 => self.op_setfl(FLAG_DEC, true),
            0x58 => self.op_setfl(FLAG_INTR, false),
            0x78 => self.op_setfl(FLAG_INTR, true),
            0xB8 => self.op_setfl(FLAG_OF, false),

            /* No-op */
            0xEA => self.op_nop(),

            _ => self.op_hlt(),
        }
    }

    //
    // Opcode implementations
    //

    fn op_load(&mut self, addr_mode: AddrMode, src_reg: RegMod) -> u32 {
        0
    }

    fn op_store(&mut self, addr_mode: AddrMode, dst_reg: RegMod) -> u32 {
        0
    }

    fn op_tx(&mut self, src_reg: RegMod, dst_reg: RegMod) -> u32 {
        0
    }

    fn op_or(&mut self, addr_mode: AddrMode) -> u32 {
        0
    }

    fn op_and(&mut self, addr_mode: AddrMode) -> u32 {
        0
    }

    fn op_xor(&mut self, addr_mode: AddrMode) -> u32 {
        0
    }

    fn op_adc(&mut self, addr_mode: AddrMode) -> u32 {
        0
    }

    fn op_sbc(&mut self, addr_mode: AddrMode) -> u32 {
        0
    }

    fn op_cmp(&mut self, addr_mode: AddrMode, src_reg: RegMod) -> u32 {
        0
    }

    fn op_dec(&mut self, addr_mode: AddrMode, dst_reg: RegMod) -> u32 {
        0
    }

    fn op_inc(&mut self, addr_mode: AddrMode, dst_reg: RegMod) -> u32 {
        0
    }

    fn op_shift(&mut self, addr_mode: AddrMode, left: bool) -> u32 {
        0
    }

    fn op_rot(&mut self, addr_mode: AddrMode, left: bool) -> u32 {
        0
    }

    fn op_pull(&mut self, dst_reg: RegMod) -> u32 {
        0
    }

    fn op_push(&mut self, src_reg: RegMod) -> u32 {
        0
    }

    fn op_jump(&mut self, test_flag: u8, branch_if: bool) -> u32 {
        0
    }

    fn op_brk(&mut self) -> u32 {
        0
    }

    fn op_rti(&mut self) -> u32 {
        0
    }

    fn op_jsr(&mut self) -> u32 {
        0
    }

    fn op_rts(&mut self) -> u32 {
        0
    }

    fn op_jmp(&mut self, addrm: AddrMode) -> u32 {
        0
    }

    fn op_bit(&mut self, addrm: AddrMode) -> u32 {
        0
    }

    fn op_setfl(&mut self, dst_flag: u8, f: bool) -> u32 {
        0
    }

    fn op_nop(&mut self) -> u32 {
        0
    }

    fn op_hlt(&mut self) -> u32 {
        // Halt processor.
        0
    }
}


#[cfg(test)]
mod tests {
    #[test]
    fn load_tests() {
        let mut cpu = Cpu::new();
        
    }
}