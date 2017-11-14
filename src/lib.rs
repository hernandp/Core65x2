//#[cfg(test)]
//mod tests;
pub mod opc6502;

// 
// Implement this memory interface 
//
pub trait Memory {
    fn read_byte(&self, addr: u16) -> u8;
    fn write_byte(&mut self, addr: u16, v: u8);    
}

use self::opc6502::OPCODE_TABLE;
use self::opc6502::CLK_TABLE;
use self::opc6502::Instr;

const STACK_ADDR_BASE: u16 = 0x0100;

//
// Processor Status Register flags
//
pub const FLAG_CARRY: u8 = 0b0000_0001;
pub const FLAG_ZERO: u8  = 0b0000_0010;
pub const FLAG_INTR: u8  = 0b0000_0100;
pub const FLAG_DEC: u8   = 0b0000_1000;
pub const FLAG_BRK: u8   = 0b0001_0000;
pub const FLAG_RSVD: u8  = 0b0010_0000;
pub const FLAG_OF: u8    = 0b0100_0000;
pub const FLAG_SIGN: u8  = 0b1000_0000;

//
// Vector adresses
//
const VECTOR_NMI: u16 = 0xFFFA;
const VECTOR_RESET: u16 = 0xFFFC;
const VECTOR_IRQ_BRK: u16 = 0xFFFE;


//
// Addressing mode modifier for opcodes
//
pub enum AddrMode {
    Acc, // Accumulator
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


// 6502 CPU registers
pub struct Regs {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub pc: u16,
    pub sp: u8,
    pub sr: u8,
}

struct InstrCycleData {
    opcode: u8,
    addr: u16,
    ea: u16,
    op0: u8,
    op1: u8,
}

pub enum InstrExecResult {
    Ok,
    Break,
    InvalidOpcode,
}

struct InterruptTraps<'a> {
    brk_trap: Option<&'a Fn()>,
    invop_trap: Option<&'a Fn()>,
}

pub struct Cpu<'a> {
    pub regs: Regs,
    pub mem: &'a mut Memory,
    clk_count: u64,
    icd: InstrCycleData,
    traps: InterruptTraps<'a>,
}


// Help macros for opcodes

macro_rules! branch_op {
    ($self:ident, $flag:ident, $cond:expr) => {
        if $self.is_flag_on($flag) == $cond {
                    $self.clk_count += 1;
                    let prevpc = $self.regs.pc;
                    $self.regs.pc = $self.regs.pc.wrapping_add($self.icd.ea);
                    if prevpc & 0xFF00 != $self.regs.pc & 0xFF00 {
                        $self.clk_count += 1;
                    }
                }
    }
}

impl<'a> Cpu<'a> {
    //
    // Initialize processor
    //
    pub fn new(sysmem: &'a mut Memory) -> Cpu<'a> {
        Cpu {
            regs: Regs {
                a: 0,
                x: 0,
                y: 0,
                sp: 0,
                pc: 0,
                sr: FLAG_RSVD,
            },
            clk_count: 0,
            mem: sysmem,
            icd: InstrCycleData {
                addr: 0x0000,
                opcode: 0x00,
                ea: 0x0000,
                op0: 0x00,
                op1: 0x00,
            },
            traps: InterruptTraps { brk_trap: None, invop_trap: None },
        }
    }


    //
    // Do the RESET cycle for the emulated processor
    //
    pub fn reset(&mut self) {
        self.regs.x = 0;
        self.regs.y = 0;
        self.regs.a = 0;
        self.regs.sr = 0b0010_0000;

        self.regs.pc = self.addr_from_2b(
            self.mem.read_byte(VECTOR_RESET),
            self.mem.read_byte(VECTOR_RESET + 1),
        );
        self.regs.sp = 0xFD;
        self.clk_count = 7;
    }
    //
    // Trigger NMI
    //
    pub fn nmi(&mut self) {
        self.push_pc();
        self.set_b_flag(false);
        self.push_sr();
        self.set_i_flag(true);
        self.regs.pc = self.addr_from_2b(
            self.mem.read_byte(VECTOR_NMI),
            self.mem.read_byte(VECTOR_NMI + 1),
        );
    }

    //
    // Trigger IRQ
    //
    pub fn irq(&mut self) {
        self.push_pc();
        self.set_b_flag(false);
        self.push_sr();
        self.set_i_flag(true);
        self.regs.pc = self.addr_from_2b(
            self.mem.read_byte(VECTOR_IRQ_BRK),
            self.mem.read_byte(VECTOR_IRQ_BRK + 1),
        );
    }

    //
    // Execute instruction at current program counter.
    // Returns elapsed clock cycles.
    //
    pub fn exec(&mut self) -> u64 {
        // Fetch instruction, operands and calculate effective address
        self.fetch_instr();
        let addr_m = &OPCODE_TABLE[self.icd.opcode as usize].addr_m;

        self.fetch_op(&addr_m);
        self.calc_eff_addr(&addr_m);

        // Add up clock cycles for execution

        self.clk_count += CLK_TABLE[self.icd.opcode as usize];

        match OPCODE_TABLE[self.icd.opcode as usize].ins {
            Instr::LDA => {
                let v = self.get_src_value(&addr_m);
                self.set_nz_flags(v);
                self.regs.a = v;
            }
            Instr::LDX => {
                let v = self.get_src_value(&addr_m);
                self.set_nz_flags(v);
                self.regs.x = v;
            }
            Instr::LDY => {
                let v = self.get_src_value(&addr_m);
                self.set_nz_flags(v);
                self.regs.y = v;
            }
            Instr::STA => self.mem.write_byte(self.icd.ea, self.regs.a),
            Instr::STX => self.mem.write_byte(self.icd.ea, self.regs.x),
            Instr::STY => self.mem.write_byte(self.icd.ea, self.regs.y),
            Instr::TXA => {
                let v = self.regs.x;
                self.regs.a = v;
                self.set_nz_flags(v);
            }
            Instr::TAX => {
                let v = self.regs.a;
                self.regs.x = v;
                self.set_nz_flags(v);
            }
            Instr::TYA => {
                let v = self.regs.y;
                self.regs.a = v;
                self.set_nz_flags(v);
            }
            Instr::TAY => {
                let v = self.regs.a;
                self.regs.y = v;
                self.set_nz_flags(v);
            }
            Instr::TSX => {
                let v = self.regs.sp;
                self.regs.x = v;
                self.set_nz_flags(v);
            }
            Instr::TXS => {
                let v = self.regs.x;
                self.regs.sp = v;
            }
            Instr::DEC => {
                let v = self.get_src_value(&addr_m).wrapping_sub(1);
                self.set_nz_flags(v);
                self.mem.write_byte(self.icd.ea, v);
            }
            Instr::DEX => {
                let v = self.regs.x.wrapping_sub(1);
                self.set_nz_flags(v);
                self.regs.x = v;
            }
            Instr::DEY => {
                let v = self.regs.y.wrapping_sub(1);
                self.set_nz_flags(v);
                self.regs.y = v;
            }
            Instr::INC => {
                let v = self.get_src_value(&addr_m).wrapping_add(1);
                self.set_nz_flags(v);
                self.mem.write_byte(self.icd.ea, v);
            }
            Instr::INY => {
                let v = self.regs.y.wrapping_add(1);
                self.set_nz_flags(v);
                self.regs.y = v;
            }
            Instr::INX => {
                let v = self.regs.x.wrapping_add(1);
                self.set_nz_flags(v);
                self.regs.x = v;
            }
            Instr::JMP => {
                self.regs.pc = self.icd.ea;
            }
            Instr::NOP => {
                // do nothing...
            }
            Instr::CMP => {
                let v = self.get_src_value(&addr_m);
                let cmps: u16 = (self.regs.a as u16).wrapping_sub(v as u16);
                self.set_c_flag(cmps < 0x100);
                self.set_nz_flags(cmps as u8);
            }
            Instr::CPX => {
                let v = self.get_src_value(&addr_m);
                let cmps: u16 = (self.regs.x as u16).wrapping_sub(v as u16);
                self.set_c_flag(cmps < 0x100);
                self.set_nz_flags(cmps as u8);
            }
            Instr::CPY => {
                let v = self.get_src_value(&addr_m);
                let cmps: u16 = (self.regs.y as u16).wrapping_sub(v as u16);
                self.set_c_flag(cmps < 0x100);
                self.set_nz_flags(cmps as u8);
            }
            Instr::AND => {
                let v = self.get_src_value(&addr_m) & self.regs.a;
                self.set_nz_flags(v);
                self.regs.a = v;
            }
            Instr::EOR => {
                let v = self.get_src_value(&addr_m) ^ self.regs.a;
                self.set_nz_flags(v);
                self.regs.a = v;
            }
            Instr::ORA => {
                let v = self.get_src_value(&addr_m) | self.regs.a;
                self.set_nz_flags(v);
                self.regs.a = v;
            }
            Instr::JSR => {
                let pch: u8 = ((self.regs.pc - 1) >> 8) as u8;
                let pcl: u8 = ((self.regs.pc - 1) & 0xFF) as u8;
                self.push_stack(pch);
                self.push_stack(pcl);
                self.regs.pc = self.icd.ea;
            }
            Instr::RTS => {
                let pcl: u8 = self.pop_stack();
                let pch: u8 = self.pop_stack();
                self.regs.pc = (((pch as u16) << 8) | pcl as u16).wrapping_add(1);
            }
            Instr::BRK => {
                self.regs.pc += 1;
                self.push_pc();
                self.set_b_flag(true);
                self.push_sr();
                self.set_i_flag(true);
                self.regs.pc = self.addr_from_2b(
                    self.mem.read_byte(VECTOR_IRQ_BRK),
                    self.mem.read_byte(VECTOR_IRQ_BRK + 1),
                );
                if self.traps.invop_trap.is_some() { self.traps.brk_trap.unwrap()() }
            }
            Instr::RTI => {
                self.regs.sr = self.pop_stack();
                let retaddrl = self.pop_stack();
                let retaddrh = self.pop_stack();
                self.regs.pc = ((retaddrh as u16) << 8) | retaddrl as u16;
            }
            Instr::PLA => {
                let v = self.pop_stack();
                self.regs.a = v;
                self.set_nz_flags(v);
            }
            Instr::PHA => {
                let v = self.regs.a;
                self.push_stack(v);
            }
            Instr::PLP => {
                let v = self.pop_stack();
                self.regs.sr = v | FLAG_RSVD;
            }
            Instr::PHP => {
                let v = self.regs.sr | FLAG_BRK;
                self.push_stack(v);
            }
            Instr::ADC => {
                let v = self.get_src_value(&addr_m);
                let mut add_res = self.regs.a as u16 + v as u16 + (self.regs.sr & FLAG_CARRY) as u16;

                if self.is_flag_on(FLAG_DEC) {
                    if (self.regs.a & 0xF) + (v & 0xF) + (self.regs.sr & FLAG_CARRY) > 9 {
                        add_res += 6;
                    }
                    if add_res > 0x99 {
                        add_res += 96;
                    }
                    self.set_c_flag(add_res > 0x99);
                } 
                else {
                    self.set_c_flag(add_res > 0xFF);
                }

                let ac_sign = self.regs.a & 0x80;
                let v_sign  = v & 0x80;
                let res_sign = (add_res as u8) & 0x80;                
                let of_check: bool = (ac_sign == v_sign) && (res_sign != ac_sign);
                self.set_v_flag(of_check);
                self.regs.a = add_res as u8;
                self.set_nz_flags(add_res as u8);
            }
            Instr::SBC => {
                let v = self.get_src_value(&addr_m);
                let cf = self.regs.sr & FLAG_CARRY;
                let mut sub_res = self.regs.a as u16 + (v as u16 ^ 0x00FF) + cf as u16;

                if self.is_flag_on(FLAG_DEC) {
                    if self.regs.a & 0xF < (1-cf) + v & 0xF {
                        sub_res -= 0x6;
                    }

                    if self.regs.a < (1-cf) + v {
                        sub_res -= 0x60;
                    }
                    self.set_c_flag(sub_res > 0x99);
                } 
                else {
                    self.set_c_flag(sub_res > 0xFF);
                }
               
                let ac_sign = self.regs.a & 0x80;
                let v_sign  = v & 0x80;
                let res_sign = (sub_res as u8) & 0x80;                
                let of_check: bool = (ac_sign == 0 && v_sign == 0x80 && res_sign == 0x80) ||
                                     (ac_sign == 0x80 && v_sign == 0 && res_sign == 0x00 );
                self.set_v_flag(of_check);
                self.regs.a = sub_res as u8;                
                self.set_nz_flags(sub_res as u8);
            }
            Instr::ASL => {
                let mut v = self.get_src_value(&addr_m);
                self.set_c_flag(v & 0x80 == 0x80);
                v = v << 1;
                self.set_nz_flags(v);
                self.store_value(&addr_m, v);
            }
            Instr::LSR => {
                let mut v = self.get_src_value(&addr_m);
                self.set_c_flag(v & 1 == 1);
                v = v >> 1;
                self.set_nz_flags(v);
                self.store_value(&addr_m, v);
            }
            Instr::ROR => {
                let v = self.get_src_value(&addr_m);
                let res = (v >> 1) | (self.regs.sr & FLAG_CARRY) << 7;
                self.set_c_flag(v & 1 == 1);
                self.set_nz_flags(res);
                self.store_value(&addr_m, res);
            }
            Instr::ROL => {
                let v = self.get_src_value(&addr_m);
                let res = (v << 1) | (self.regs.sr & FLAG_CARRY);
                self.set_c_flag(v & 0x80 == 0x80);
                self.set_nz_flags(res);
                self.store_value(&addr_m, res);
            }
            Instr::BIT => {
                let v = self.get_src_value(&addr_m);
                let tst = v & self.regs.a;
                self.set_s_flag(v);
                self.set_v_flag(v & 0x40 == 0x40);
                self.set_z_flag(tst);
            }
            Instr::BNE => branch_op!(self, FLAG_ZERO, false),
            Instr::BEQ => branch_op!(self, FLAG_ZERO, true),
            Instr::BCC => branch_op!(self, FLAG_CARRY, false),
            Instr::BCS => branch_op!(self, FLAG_CARRY, true),
            Instr::BVC => branch_op!(self, FLAG_OF, false),
            Instr::BVS => branch_op!(self, FLAG_OF, true),
            Instr::BPL => branch_op!(self, FLAG_SIGN, false),
            Instr::BMI => branch_op!(self, FLAG_SIGN, true),
            Instr::SEC => self.regs.sr |= FLAG_CARRY,
            Instr::SEI => self.regs.sr |= FLAG_INTR,
            Instr::SED => self.regs.sr |= FLAG_DEC,
            Instr::CLC => self.regs.sr &= !FLAG_CARRY,
            Instr::CLI => self.regs.sr &= !FLAG_INTR,
            Instr::CLD => self.regs.sr &= !FLAG_DEC,
            Instr::CLV => self.regs.sr &= !FLAG_OF,
            Instr::INVALID => { if self.traps.invop_trap.is_some() { self.traps.invop_trap.unwrap()() } }
        }

        CLK_TABLE[self.icd.opcode as usize]
    }

    // Gets instruction length by addressing mode
    //
    pub fn get_instr_length(&self, addrm: &AddrMode) -> u16 {
        match *addrm {
            AddrMode::Imm |
            AddrMode::ZP |
            AddrMode::ZPX |
            AddrMode::ZPY |
            AddrMode::ZPIndX |
            AddrMode::ZPIndY |
            AddrMode::Rel => 2,
            AddrMode::Abs | AddrMode::AbsX | AddrMode::AbsY | AddrMode::Ind => 3,
            _ => 1,
        }
    }

    //
    // Fetch instruction and increment PC
    //
    fn fetch_instr(&mut self) {
        self.icd.opcode = self.mem.read_byte(self.regs.pc);
        self.icd.addr = self.regs.pc;
        self.regs.pc += 1;
    }

    //
    // Fetch operands and increment PC
    //
    fn fetch_op(&mut self, addr_mode: &AddrMode) {
        let num_operands = self.get_instr_length(addr_mode) - 1;

        if num_operands != 0 {
            self.regs.pc = self.icd.addr + 2;
            self.icd.op0 = self.mem.read_byte(self.icd.addr + 1);
        }
        if num_operands == 2 {
            self.regs.pc += 1;
            self.icd.op1 = self.mem.read_byte(self.icd.addr + 2);
        }
    }

    pub fn is_flag_on(&self, flag: u8) -> bool {
        self.regs.sr & flag == flag
    }

    fn set_s_flag(&mut self, v: u8) {
        if v >= 0x80 {
            self.regs.sr |= FLAG_SIGN;
        } else {
            self.regs.sr &= !FLAG_SIGN;
        }
    }

    fn set_z_flag(&mut self, v: u8) {
        if v == 0 {
            self.regs.sr |= FLAG_ZERO;
        } else {
            self.regs.sr &= !FLAG_ZERO;
        }
    }

    fn set_c_flag(&mut self, v: bool) {
        if v {
            self.regs.sr |= FLAG_CARRY;
        } else {
            self.regs.sr &= !FLAG_CARRY;
        }
    }

    fn set_b_flag(&mut self, v: bool) {
        if v {
            self.regs.sr |= FLAG_BRK;
        } else {
            self.regs.sr &= !FLAG_BRK;
        }
    }

    fn set_i_flag(&mut self, v: bool) {
        if v {
            self.regs.sr |= FLAG_INTR;
        } else {
            self.regs.sr &= !FLAG_INTR;
        }
    }

    fn set_v_flag(&mut self, v: bool) {
        if v {
            self.regs.sr |= FLAG_OF;
        } else {
            self.regs.sr &= !FLAG_OF;
        }
    }

    // fn set_d_flag(&mut self, v: bool) {
    //     if v {
    //         self.regs.sr |= FLAG_DEC;
    //     } else {
    //         self.regs.sr &= !FLAG_DEC;
    //     }
    // }

    fn set_nz_flags(&mut self, v: u8) {
        self.set_z_flag(v);
        self.set_s_flag(v);
    }

    fn addr_from_2b(&self, b0: u8, b1: u8) -> u16 {
        ((b1 as u16) << 8) | (b0 as u16)
    }

    fn calc_eff_addr(&mut self, addr_mode: &AddrMode) {
        match *addr_mode {
            AddrMode::Acc | AddrMode::Impl => {}
            AddrMode::Imm => self.icd.ea = self.icd.addr + 1,
            AddrMode::ZP => self.icd.ea = self.icd.op0 as u16,
            AddrMode::ZPX => self.icd.ea = self.icd.op0.wrapping_add(self.regs.x) as u16,
            AddrMode::ZPY => self.icd.ea = self.icd.op0.wrapping_add(self.regs.y) as u16,
            AddrMode::Abs => self.icd.ea = self.addr_from_2b(self.icd.op0, self.icd.op1),
            AddrMode::AbsX => {
                if (self.regs.x).overflowing_add(self.icd.op0).1 {
                    self.clk_count += 1;
                };

                self.icd.ea = (self.regs.x as u16)
                    .wrapping_add(self.addr_from_2b(self.icd.op0, self.icd.op1));
            }

            AddrMode::AbsY => {
                if (self.regs.y).overflowing_add(self.icd.op0).1 {
                    self.clk_count += 1;
                };
                self.icd.ea = (self.regs.y as u16)
                    .wrapping_add(self.addr_from_2b(self.icd.op0, self.icd.op1));
            }

            AddrMode::ZPIndX => {
                self.icd.ea = self.addr_from_2b(
                    self.mem
                        .read_byte(self.regs.x.wrapping_add(self.icd.op0) as u16),
                    self.mem
                        .read_byte(self.regs.x.wrapping_add(1).wrapping_add(self.icd.op0)
                            as u16),
                )
            }

            AddrMode::ZPIndY => {
                let indirect_addr = self.addr_from_2b(
                    self.mem.read_byte(self.icd.op0 as u16),
                    self.mem.read_byte(self.icd.op0.wrapping_add(1) as u16),
                );

                if ((indirect_addr & 0xFF) as u8)
                    .overflowing_add(self.regs.y)
                    .1
                {
                    self.clk_count += 1;
                }

                self.icd.ea = indirect_addr.wrapping_add(self.regs.y as u16);
            }
            AddrMode::Ind => {
                let indirect_addr = self.addr_from_2b(self.icd.op0, self.icd.op1);
                self.icd.ea = self.addr_from_2b(
                    self.mem.read_byte(indirect_addr),
                    self.mem.read_byte(indirect_addr.wrapping_add(1)),
                );
            }
            /* For relative-addressing mode, EA should be interpreted as 16-bit,
               signed relative-address */
            AddrMode::Rel => {
                self.icd.ea = if self.icd.op0 & 0x80 == 0x80 {
                    self.icd.op0 as u16 | 0xFF00
                } else {
                    self.icd.op0 as u16
                }
            }
        }
    }

    fn push_pc(&mut self) {
        let retaddrh: u8 = (self.regs.pc >> 8) as u8;
        let retaddrl: u8 = (self.regs.pc & 0xFF) as u8;
        self.push_stack(retaddrh);
        self.push_stack(retaddrl);
    }

    fn push_sr(&mut self) {
        let sr = self.regs.sr;
        self.push_stack(sr);
    }


    fn push_stack(&mut self, v: u8) {
        self.mem.write_byte(STACK_ADDR_BASE + self.regs.sp as u16, v);
        self.regs.sp = self.regs.sp.wrapping_sub(1);
    }

    fn pop_stack(&mut self) -> u8 {
        self.regs.sp = self.regs.sp.wrapping_add(1);
        self.mem.read_byte(STACK_ADDR_BASE + self.regs.sp as u16)
    }

    fn get_src_value(&self, addr_mode: &AddrMode) -> u8 {
        match *addr_mode {
            AddrMode::Acc => self.regs.a,
            _ => self.mem.read_byte(self.icd.ea),
        }
    }

    fn store_value(&mut self, addr_mode: &AddrMode, v: u8) {
        match *addr_mode {
            AddrMode::Acc => self.regs.a = v,
            _ => self.mem.write_byte(self.icd.ea, v),
        }
    }
}
