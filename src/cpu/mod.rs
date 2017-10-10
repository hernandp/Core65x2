#[cfg(test)]
mod tests;

use super::mem::Memory;

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
// Instruction operands
//
type Operands = (u8, Option<u8>);

//
// Addressing mode modifier for opcodes
//
enum AddrMode {
    Impl,
    Imm,    // LDA #$22   Immediate
    ZP,     // LDY $02      Zero-page
    ZPX,    // LDA $00,X    Zero-page indexed (X)
    ZPY,    // LDA $00,Y    Zero-page indexed (Y)
    ZPIndX, // ($00,X)      Zero-page indexed indirect X
    ZPIndY, // ($00),Y      Zero-page indexed indirect Y
    Abs,    // LDX $0000    Absolute
    AbsX,   // ADC $C000,X  Absolute indexed with X
    AbsY,   // INC $F000,Y  Absolute indexed with Y
    Ind,    // ($0000)      Indirect
    Rel,    // PC + $00
}

// Register modifier for opcodes
enum RegMod {
    A,
    X,
    Y,
    SP,
    SR,
}

// Instruction group for address calculation
enum InstrGroup {
    Read,
    ReadWrite,
    Write,
    Jump
}

// Effective address: (address, clock count)
struct EAResult {
    addr: u16,
    clk_count: u64,
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

pub struct Cpu<'a> {
    regs: Regs,
    mem: &'a mut Memory,
    clk_count: u64,
}

impl<'a> Cpu<'a> {
    //
    // Initialize processor
    //
    pub fn new(sysmem: &'a mut Memory) -> Cpu<'a> {
        Cpu {
            regs: Regs {
                A: 0,
                X: 0,
                Y: 0,
                SP: 0,
                PC: 0,
                SR: 0,
            },
            clk_count: 0,
            mem: sysmem,
        }
    }

    //
    // Execute instruction at current program counter.
    // Returns elapsed clock cycles.
    //
    pub fn exec(&mut self) -> u64 {
        let clk = match self.fetch_instr() {
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
        };

        self.clk_count += clk;
        clk
    }

    //
    // Fetch instruction, and updates program counter
    //
    fn fetch_instr(&mut self) -> u8 {
        let opcode = self.mem.read_byte(self.regs.PC);
        self.regs.PC = self.regs.PC + 1;
        opcode
    }

    //
    // Fetch operands, updating program counter
    //
    fn fetch_op(&mut self, addr_mode: &AddrMode) -> Option<Operands> {
        let operands: Option<Operands>;
        match *addr_mode {
            AddrMode::Imm |
            AddrMode::ZP |
            AddrMode::ZPX |
            AddrMode::ZPY |
            AddrMode::ZPIndX |
            AddrMode::ZPIndY |
            AddrMode::Rel => {
                operands = Some((self.mem.read_byte(self.regs.PC), None));
                self.regs.PC += 1;
            }
            AddrMode::Abs | AddrMode::AbsX | AddrMode::AbsY | AddrMode::Ind => {
                operands = Some((
                    self.mem.read_byte(self.regs.PC),
                    Some(self.mem.read_byte(self.regs.PC + 1)),
                ));
                self.regs.PC += 2;
            }
            _ => {
                operands = None;
            }
        }
        operands
    }

    fn write_register(&mut self, dst_reg: &RegMod, v: u8) {
        match *dst_reg {
            RegMod::A => self.regs.A = v,
            RegMod::X => self.regs.X = v,
            RegMod::Y => self.regs.Y = v,
            RegMod::SR => self.regs.SR = v,
            RegMod::SP => self.regs.SP = v,
        };
    }

    fn read_register(&self, src_reg: &RegMod) -> u8 {
        match *src_reg {
            RegMod::A => self.regs.A,
            RegMod::X => self.regs.X,
            RegMod::Y => self.regs.Y,
            RegMod::SR => self.regs.SR,
            RegMod::SP => self.regs.SP,
        }
    }

    fn set_s_flag(&mut self, v: u8) {
        if v >= 0x80 {
            self.regs.SR |= FLAG_SIGN;
        } else {
            self.regs.SR &= !FLAG_SIGN;
        }
    }

    fn set_z_flag(&mut self, v: u8) {
        if v == 0 {
            self.regs.SR |= FLAG_ZERO;
        } else {
            self.regs.SR &= !FLAG_ZERO;
        }
    }

    fn set_c_flag(&mut self, v: bool) {
        if v == false {
            self.regs.SR |= FLAG_CARRY;
        } else {
            self.regs.SR &= !FLAG_CARRY;
        }
    }

    fn set_v_flag(&mut self, v: bool) {
        if v == false {
            self.regs.SR |= FLAG_CARRY;
        } else {
            self.regs.SR &= !FLAG_CARRY;
        }
    }

    fn set_d_flag(&mut self, v: bool) {
        if v == false {
            self.regs.SR |= FLAG_DEC;
        } else {
            self.regs.SR &= !FLAG_DEC;
        }
    }

    fn set_nz_flags(&mut self, flags: u8, v: u8) {
        if FLAG_ZERO == (flags & FLAG_ZERO) {
            self.set_z_flag(v);
        }
        if FLAG_SIGN == (flags & FLAG_SIGN) {
            self.set_s_flag(v);
        }
    }

    fn addr_from_2b(&self, b0: u8, b1: u8) -> u16 {
        ((b1 as u16) << 8) | (b0 as u16)
    }

    fn calc_eff_addr(&self, insgrp: &InstrGroup, addr_mode: &AddrMode, ops: &Operands) -> EAResult {
        match *addr_mode {
            AddrMode::Imm | AddrMode::Impl => {
                EAResult {
                    addr: 0, // this must be ignored by caller
                    clk_count: 2,
                }
            }
            AddrMode::ZP => EAResult {
                addr: (*ops).0 as u16,
                clk_count: match *insgrp {
                    InstrGroup::Read | InstrGroup::Write => 3,
                    InstrGroup::ReadWrite => 5,
                },
            },
            AddrMode::ZPX => EAResult {
                addr: (*ops).0.wrapping_add(self.regs.X) as u16,
                clk_count: match *insgrp {
                    InstrGroup::Read | InstrGroup::Write => 4,
                    InstrGroup::ReadWrite => 6,
                },
            },
            AddrMode::ZPY => EAResult {
                addr: (*ops).0.wrapping_add(self.regs.Y) as u16,
                clk_count: match *insgrp {
                    InstrGroup::Read | InstrGroup::Write => 4,
                    InstrGroup::ReadWrite => 6,
                },
            },
            AddrMode::Abs => EAResult {
                addr: self.addr_from_2b((*ops).0, (*ops).1.unwrap()),
                clk_count: match *insgrp {
                    InstrGroup::Read | InstrGroup::Write => 4,
                    InstrGroup::ReadWrite => 6,
                },
            },
            AddrMode::AbsX => {
                let mut page_cross_clk = 0;
                if (self.regs.X).overflowing_add((*ops).0).1 {
                    page_cross_clk = 1;
                };

                EAResult {
                    addr: (self.regs.X as u16)
                        .wrapping_add(self.addr_from_2b((*ops).0, (*ops).1.unwrap())),

                    clk_count: match *insgrp {
                        InstrGroup::Read => 4 + page_cross_clk,
                        InstrGroup::Write => 5,
                        InstrGroup::ReadWrite => 7,
                    },
                }
            }
            AddrMode::AbsY => {
                let mut page_cross_clk = 0;
                if (self.regs.Y).overflowing_add((*ops).0).1 {
                    page_cross_clk = 1;
                };
                EAResult {
                    addr: (self.regs.Y as u16)
                        .wrapping_add(self.addr_from_2b((*ops).0, (*ops).1.unwrap())),
                    clk_count: match *insgrp {
                        InstrGroup::Read => 4 + page_cross_clk,
                        InstrGroup::Write => 5,
                        InstrGroup::ReadWrite => 7,
                    },
                }
            }
            AddrMode::ZPIndX => EAResult {
                addr: self.addr_from_2b(
                    self.mem
                        .read_byte((self.regs.X.wrapping_add((*ops).0)) as u16),
                    self.mem
                        .read_byte((self.regs.X.wrapping_add(1).wrapping_add((*ops).0)) as u16),
                ),
                clk_count: match *insgrp {
                    InstrGroup::Read | InstrGroup::Write => 6,
                    InstrGroup::ReadWrite => 8,
                },
            },
            AddrMode::ZPIndY => {
                let indirect_addr = self.addr_from_2b(
                    self.mem.read_byte((*ops).0 as u16),
                    self.mem.read_byte((*ops).0.wrapping_add(1) as u16),
                );

                let mut page_cross_clk = 0;
                if ((indirect_addr & 0xFF) as u8)
                    .overflowing_add(self.regs.Y)
                    .1
                {
                    page_cross_clk = 1;
                }

                EAResult {
                    addr: indirect_addr.wrapping_add(self.regs.Y as u16),
                    clk_count: match *insgrp {
                        InstrGroup::Read => 6 + page_cross_clk,
                        InstrGroup::Write => 6,
                        InstrGroup::ReadWrite => 8,
                    },
                }
            }
            AddrMode::Ind => {
                let indirect_addr = self.addr_from_2b((*ops).0, (*ops).1.unwrap());
                EAResult {
                    addr: self.addr_from_2b(
                        self.mem.read_byte(indirect_addr),
                        self.mem.read_byte(indirect_addr.wrapping_add(1)),
                    ),
                    clk_count: 5,
                }
            }
            AddrMode::Rel => {
                // TODO
                EAResult {
                    addr: self.regs.PC + (*ops).0 as u16,
                    clk_count: 5,
                }
            }
        }
    }

    fn push_stack(&mut self, v: u8) {
        self.mem.write_byte(self.regs.SP as u16, v);
        self.regs.SP = self.regs.SP - 1;
    }

    fn pop_stack(&mut self) -> u8 {
        self.regs.SP = self.regs.SP + 1;
        self.mem.read_byte(self.regs.SP as u16)
    }

    // -----------------------------------------------------------------------------------------------------
    //
    // Opcode implementations
    //
    // -----------------------------------------------------------------------------------------------------

    // This macro will generate the code for fetching operands, calculating effective address/clocks,
    // and optionally reading operand value from EA
    // //
    // macro_rules! fetch_op_ea {
    //     ($ops:ident, $addrmode, $ea:ident, $v:ident) => {
    //          let $ops = self.fetch_op(&addr_mode);
    //          let $ea: EAResult = self.calc_eff_addr(&InstrGroup::Read, &addr_mode, &ops.unwrap());

    //         let v = match addr_mode {
    //             AddrMode::Imm => ops.unwrap().0,
    //         _ => self.mem.read_byte(ea.addr),
    //     };

            
    //     };
    // }

    fn op_load(&mut self, addr_mode: AddrMode, src_reg: RegMod) -> u64 {
        let ops = self.fetch_op(&addr_mode);
        let ea: EAResult = self.calc_eff_addr(&InstrGroup::Read, &addr_mode, &ops.unwrap());
        let v = match addr_mode {
            AddrMode::Imm => ops.unwrap().0,
            _ => self.mem.read_byte(ea.addr),
        };

        self.set_nz_flags(FLAG_SIGN | FLAG_ZERO, v);
        self.write_register(&src_reg, v);

        ea.clk_count
    }

    fn op_store(&mut self, addr_mode: AddrMode, dst_reg: RegMod) -> u64 {
        let ops = self.fetch_op(&addr_mode);
        let ea: EAResult = self.calc_eff_addr(&InstrGroup::Write, &addr_mode, &ops.unwrap());
        let v = self.read_register(&dst_reg);
        self.mem.write_byte(ea.addr, v);
        ea.clk_count
    }

    fn op_tx(&mut self, src_reg: RegMod, dst_reg: RegMod) -> u64 {
        let v = self.read_register(&src_reg);
        self.write_register(&dst_reg, v);
        self.set_nz_flags(FLAG_SIGN | FLAG_ZERO, v);
        2
    }

    fn op_or(&mut self, addr_mode: AddrMode) -> u64 {
        let ops = self.fetch_op(&addr_mode);
        let ea: EAResult = self.calc_eff_addr(&InstrGroup::ReadWrite, &addr_mode, &ops.unwrap());
        let v = self.mem.read_byte(ea.addr);
        self.mem.write_byte(ea.addr, v | self.regs.A);
        self.set_nz_flags(FLAG_SIGN | FLAG_ZERO, v);
        ea.clk_count
    }

    fn op_and(&mut self, addr_mode: AddrMode) -> u64 {
        let ops = self.fetch_op(&addr_mode);
        let ea: EAResult = self.calc_eff_addr(&InstrGroup::ReadWrite, &addr_mode, &ops.unwrap());
        let v = self.mem.read_byte(ea.addr);
        self.mem.write_byte(ea.addr, v & self.regs.A);
        self.set_nz_flags(FLAG_SIGN | FLAG_ZERO, v);
        ea.clk_count
    }

    fn op_xor(&mut self, addr_mode: AddrMode) -> u64 {
        let ops = self.fetch_op(&addr_mode);
        let ea: EAResult = self.calc_eff_addr(&InstrGroup::ReadWrite, &addr_mode, &ops.unwrap());
        let v = self.mem.read_byte(ea.addr);
        self.mem.write_byte(ea.addr, v ^ self.regs.A);
        self.set_nz_flags(FLAG_SIGN | FLAG_ZERO, v);
        ea.clk_count
    }

    fn op_adc(&mut self, addr_mode: AddrMode) -> u64 {
        let ops = self.fetch_op(&addr_mode);
        let ea: EAResult = self.calc_eff_addr(&InstrGroup::Read, &addr_mode, &ops.unwrap());
        let v = match addr_mode {
            AddrMode::Imm => ops.unwrap().0,
            _ => self.mem.read_byte(ea.addr),
        };
        
        let add_res: u32 = self.regs.A as u32 + v as u32 + if self.regs.SR & FLAG_CARRY == FLAG_CARRY { 1u32 } else { 0u32 };
        if add_res > 0xFF {
            self.set_c_flag(true);
        }
        let of_check: bool = ((self.regs.A ^ v) & 0x80) != 0 && ((self.regs.A ^ v) & 0x80) != 0;
        self.set_v_flag(!of_check);

        if self.regs.SR & FLAG_DEC == FLAG_DEC {
            // Support decimal
        }

        self.regs.A = add_res as u8;
        self.set_nz_flags(FLAG_SIGN | FLAG_ZERO, add_res as u8);
        ea.clk_count
    }

    fn op_sbc(&mut self, addr_mode: AddrMode) -> u64 {
        let ops = self.fetch_op(&addr_mode);
        let ea: EAResult = self.calc_eff_addr(&InstrGroup::Read, &addr_mode, &ops.unwrap());
        let v = match addr_mode {
            AddrMode::Imm => ops.unwrap().0,
            _ => self.mem.read_byte(ea.addr),
        };
        
        let sbc_res: u32 = self.regs.A as u32 - v as u32 - if self.regs.SR & FLAG_CARRY == FLAG_CARRY { 0u32 } else { 1u32 };
        if sbc_res < 0x100 {
            self.set_c_flag(true);
        }
        let of_check: bool = ((self.regs.A ^ v) & 0x80) != 0 && ((self.regs.A ^ v) & 0x80) != 0;
        self.set_v_flag(of_check);

        if self.regs.SR & FLAG_DEC == FLAG_DEC {
            // Support decimal
        }

        self.regs.A = sbc_res as u8;
        self.set_nz_flags(FLAG_SIGN | FLAG_ZERO, sbc_res as u8);
        ea.clk_count        
    }

    fn op_cmp(&mut self, addr_mode: AddrMode, src_reg: RegMod) -> u64 {
        let ops = self.fetch_op(&addr_mode);
        let ea: EAResult = self.calc_eff_addr(&InstrGroup::Read, &addr_mode, &ops.unwrap());
        let v = match addr_mode {
            AddrMode::Imm => ops.unwrap().0,
            _ => self.mem.read_byte(ea.addr),
        };

        let cmp_res: u32 = self.regs.A as u32 - v as u32;
        if cmp_res < 0x100 {
            self.set_c_flag(true);
        }

        self.set_nz_flags(FLAG_SIGN | FLAG_ZERO, cmp_res as u8);
        ea.clk_count
    }

    fn op_dec(&mut self, addr_mode: AddrMode, dst_reg: RegMod) -> u64 {
        let ops = self.fetch_op(&addr_mode);
        let ea: EAResult = self.calc_eff_addr(&InstrGroup::ReadWrite, &addr_mode, &ops.unwrap());
        let v = self.mem.read_byte(ea.addr);
        self.mem.write_byte(ea.addr, v.wrapping_sub(1));
        self.set_nz_flags(FLAG_SIGN | FLAG_ZERO, v);
        ea.clk_count
    }

    fn op_inc(&mut self, addr_mode: AddrMode, dst_reg: RegMod) -> u64 {
        let ops = self.fetch_op(&addr_mode);
        let ea: EAResult = self.calc_eff_addr(&InstrGroup::ReadWrite, &addr_mode, &ops.unwrap());
        let v = self.mem.read_byte(ea.addr);
        self.mem.write_byte(ea.addr, v.wrapping_add(1));
        self.set_nz_flags(FLAG_SIGN | FLAG_ZERO, v);
        ea.clk_count
    }

    fn op_shift(&mut self, addr_mode: AddrMode, left: bool) -> u64 {
        0
    }

    fn op_rot(&mut self, addr_mode: AddrMode, left: bool) -> u64 {
        0
    }

    fn op_pull(&mut self, dst_reg: RegMod) -> u64 {
        
    }

    fn op_push(&mut self, src_reg: RegMod) -> u64 {
        0
    }

    fn op_jump(&mut self, test_flag: u8, branch_if: bool) -> u64 {
        0
    }

    fn op_brk(&mut self) -> u64 {
        0
    }

    fn op_rti(&mut self) -> u64 {
        0
    }

    fn op_jsr(&mut self) -> u64 {
        let ops = self.fetch_op(&AddrMode::Abs);
        self.regs.PC -= 1;
        let pch: u8 = (self.regs.PC >> 8) as u8;
        let pcl: u8 = (self.regs.PC & 0xFF) as u8;
        self.push_stack(pch);
        self.push_stack(pcl);
        6
    }

    fn op_rts(&mut self) -> u64 {
        let pcl: u8 = self.pop_stack();
        let pch: u8 = self.pop_stack();
        self.regs.PC = (((pch as u16) << 8) | pcl as u16).wrapping_add(1);
        6
    }

    fn op_jmp(&mut self, addr_mode: AddrMode) -> u64 {
        let ops = self.fetch_op(&addr_mode);
        let ea: EAResult = self.calc_eff_addr(&InstrGroup::Jump, &addr_mode, &ops.unwrap());
        self.regs.PC = ea.addr;
        ea.clk_count
    }

    fn op_bit(&mut self, addrm: AddrMode) -> u64 {
        0
    }

    fn op_setfl(&mut self, dst_flag: u8, f: bool) -> u64 {
        /*self.set_flags(&dst_flag, f); */
        2
    }

    fn op_nop(&mut self) -> u64 {
        2
    }

    fn op_hlt(&mut self) -> u64 {
        // Halt processor.
        0
    }
}
