#[cfg(test)]
mod tests;
pub mod opc6502;

use super::mem::Memory;
use self::opc6502::OPCODE_TABLE;
use self::opc6502::CLK_TABLE;

const STACK_ADDR_BASE: u16 = 0x01FF;
const STACK_ADDR_LIMIT: u16 = 0x0100;

//
// Processor Status Register flags
//
pub const FLAG_CARRY: u8 = 0b0000_0001;
pub const FLAG_ZERO: u8 = 0b0000_0010;
pub const FLAG_INTR: u8 = 0b0000_0100;
pub const FLAG_DEC: u8 = 0b0000_1000;
pub const FLAG_BRK: u8 = 0b0001_0000;
pub const FLAG_OF: u8 = 0b0100_0000;
pub const FLAG_SIGN: u8 = 0b1000_0000;

// 
// Vector adresses
//
const VECTOR_NMI: u16 = 0xFFFA;
const VECTOR_RESET:u16= 0xFFFC;
const VECTOR_IRQ_BRK:  u16= 0xFFFE;


//
// Addressing mode modifier for opcodes
//
pub enum AddrMode {
    Acc,    // Accumulator
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
#[derive(PartialEq)]
pub enum RegMod {
    None,
    A,
    X,
    Y,
    SP,
    SR,
}

// Instruction R/W type for address calculation
enum InsMemAccess {
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
pub struct Regs {
    pub A: u8,
    pub X: u8,
    pub Y: u8,
    pub PC: u16,
    pub SP: u8,
    pub SR: u8,
}

struct InstrCycleData {
    opcode: u8,
    ea: u16,
    op0: u8,
    op1: u8,
}

pub struct Cpu<'a> {
    pub regs: Regs,
    pub mem: &'a mut Memory,
    clk_count: u64,
    icd: InstrCycleData, 
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
            icd: InstrCycleData {
                opcode: 0x00,
                ea: 0x0000, 
                op0: 0x00,
                op1: 0x00
            }
        }
    }

    // 
    // Do the RESET cycle for the emulated processor
    //
    pub fn reset(&mut self) {
        self.regs.PC = self.addr_from_2b(self.mem.read_byte(VECTOR_RESET),self.mem.read_byte(VECTOR_RESET + 1));
        self.regs.SP = 0xFD;
        self.clk_count = 7;
    }

    //
    // Execute instruction at current program counter.
    // Returns elapsed clock cycles.
    //
    pub fn exec(&mut self)  {

        // Fetch instruction, operands and calculate effective address

        self.fetch_instr();                
        let addr_m = &OPCODE_TABLE[self.icd.opcode as usize].addr_m;
        
        self.fetch_op(&addr_m);
        self.calc_eff_addr(&addr_m);

        match OPCODE_TABLE[self.icd.opcode as usize].name {
            "BRK" => {
                println!("BRK");
            }
            "LDA" => {
                println!("LDA");
            }
            "LDX" => {
                println!("LDX");
            }
            _ => {
                println!("OTHER");
            }
        }

        // Add up clock cycles for execution

        self.clk_count += CLK_TABLE[self.icd.opcode as usize];


/*
        let clk: u64 = match opcode_table[self.icd.opcode as usize].ins {
            InsGrp::Adc => self.op_adc(addr_m),
            InsGrp::And => self.op_and(addr_m),
            InsGrp::Bit => self.op_bit(addr_m),
            InsGrp::Brk => self.op_brk(),
            InsGrp::CJump => self.op_jump(*flag_m, *arg),
            InsGrp::Cmp => self.op_cmp(addr_m, reg_m),
            InsGrp::Dec => self.op_dec(addr_m, reg_m),
            InsGrp::Flag =>self.op_setfl(*flag_m, *arg),
            InsGrp::Inc => self.op_inc(addr_m, reg_m),
            InsGrp::Jsr => self.op_jsr(),
            InsGrp::Jump =>self.op_jmp(addr_m ),
            InsGrp::Load =>self.op_load(addr_m, reg_m),
            InsGrp::Nop => self.op_nop(),
            InsGrp::Or =>  self.op_or(addr_m),
            InsGrp::Pull =>self.op_pull(reg_m),
            InsGrp::Push =>self.op_push(reg_m),
            InsGrp::Rot => self.op_rot(addr_m, *arg),
            InsGrp::Rti => self.op_rti(),
            InsGrp::Rts => self.op_rts(),
            InsGrp::Sbc => self.op_sbc(addr_m),
            InsGrp::Shift => self.op_shift(addr_m, *arg),
            InsGrp::Store => self.op_store(addr_m, reg_m),
            InsGrp::Tx_from_A => self.op_tx(&RegMod::A, reg_m),
            InsGrp::Tx_from_SP =>self.op_tx(&RegMod::SP, reg_m),
            InsGrp::Tx_from_X => self.op_tx(&RegMod::X, reg_m),
            InsGrp::Tx_from_Y => self.op_tx(&RegMod::Y, reg_m),
            InsGrp::Xor => self.op_xor(addr_m),
            InsGrp::Invalid => self.op_hlt(),
        };        

        self.clk_count += clk;
        clk
        */        
    }

    // Gets instruction length by addressing mode
    //
    pub fn get_instr_length(&self, addrm: &AddrMode) -> u32 {
        match *addrm {
            AddrMode::Imm |
            AddrMode::ZP |
            AddrMode::ZPX |
            AddrMode::ZPY |
            AddrMode::ZPIndX |
            AddrMode::ZPIndY |
            AddrMode::Rel => 2,
            AddrMode::Abs | AddrMode::AbsX | AddrMode::AbsY | AddrMode::Ind => 3,
            _ => 1
        }
    }

    //
    // Fetch instruction, and updates program counter
    //
    fn fetch_instr(&mut self)  {
        self.icd.opcode = self.mem.read_byte(self.regs.PC);
        self.regs.PC = self.regs.PC + 1;
    }

    //
    // Fetch operands, updating program counter
    //
    fn fetch_op(&mut self, addr_mode: &AddrMode) {
        let num_operands = self.get_instr_length(addr_mode) - 1;

        if num_operands != 0 {
            self.icd.op0 = self.mem.read_byte(self.regs.PC);
        }
        if num_operands == 2 {
            self.icd.op1 = self.mem.read_byte(self.regs.PC + 1);
        }
        self.regs.PC += num_operands as u16;
    }

    fn write_register(&mut self, dst_reg: &RegMod, v: u8) {
        match *dst_reg {
            RegMod::A => self.regs.A = v,
            RegMod::X => self.regs.X = v,
            RegMod::Y => self.regs.Y = v,
            RegMod::SR => self.regs.SR = v,
            RegMod::SP => self.regs.SP = v,
            _ => panic!("invalid Reg modifier")
        };
    }

    fn read_register(&self, src_reg: &RegMod) -> u8 {
        match *src_reg {
            RegMod::A => self.regs.A,
            RegMod::X => self.regs.X,
            RegMod::Y => self.regs.Y,
            RegMod::SR => self.regs.SR,
            RegMod::SP => self.regs.SP,
            _ => panic!("invalid Reg modifier")
        }
    }

    pub fn is_flag_on(&self, flag: u8) -> bool {
        self.regs.SR & flag == flag 
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

    fn calc_eff_addr(&mut self, addr_mode: &AddrMode) {
        match *addr_mode {
            AddrMode::Acc | AddrMode::Imm => {},
            AddrMode::Impl => self.icd.ea = self.regs.PC + 1,
            AddrMode::ZP =>   self.icd.ea = self.icd.op0 as u16,
            AddrMode::ZPX =>  self.icd.ea = self.icd.op0.wrapping_add(self.regs.X) as u16,
            AddrMode::ZPY =>  self.icd.ea = self.icd.op0.wrapping_add(self.regs.Y) as u16,
            AddrMode::Abs =>  self.icd.ea = self.addr_from_2b(self.icd.op0, self.icd.op1),
            AddrMode::AbsX => {
                if (self.regs.X).overflowing_add(self.icd.op0).1 {
                self.clk_count += 1;
            };
            
                self.icd.ea = (self.regs.X as u16).wrapping_add(self.addr_from_2b(self.icd.op0, self.icd.op1));           
            },

            AddrMode::AbsY => {
                if (self.regs.Y).overflowing_add(self.icd.op0).1 {
                    self.clk_count += 1;
                };
                self.icd.ea = (self.regs.Y as u16).wrapping_add(self.addr_from_2b(self.icd.op0, self.icd.op1));                    
                },
            
            AddrMode::ZPIndX => self.icd.ea = self.addr_from_2b(
                    self.mem
                        .read_byte(self.regs.X.wrapping_add(self.icd.op0) as u16),
                    self.mem
                        .read_byte(self.regs.X.wrapping_add(1).wrapping_add(self.icd.op0) as u16)),

            AddrMode::ZPIndY => {
                let indirect_addr = self.addr_from_2b(
                    self.mem.read_byte(self.icd.op0 as u16),
                    self.mem.read_byte(self.icd.op0.wrapping_add(1) as u16)
                );

                if ((indirect_addr & 0xFF) as u8)
                    .overflowing_add(self.regs.Y)
                    .1
                {
                    self.clk_count += 1;
                }
                
                self.icd.ea = indirect_addr.wrapping_add(self.regs.Y as u16);                    
            },
            AddrMode::Ind => {
                let indirect_addr = self.addr_from_2b(self.icd.op0, self.icd.op1);
                self.icd.ea = self.addr_from_2b(
                        self.mem.read_byte(indirect_addr),
                        self.mem.read_byte(indirect_addr.wrapping_add(1)));
                },
            AddrMode::Rel => self.icd.ea = self.regs.PC + self.icd.op0 as u16
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
/*
    fn get_src_value(&self, addr_mode: &AddrMode, src_addr: u16) -> u8 {
        match *addr_mode {
            AddrMode::Acc => self.regs.A,
            _ => self.mem.read_byte(src_addr),
        }
    }

    fn op_load(&mut self, addr_mode: &AddrMode, src_reg: &RegMod) -> u64 {
        // let ops = self.fetch_op(addr_mode);
        // let ea: EAResult = self.calc_eff_addr(&InsMemAccess::Read, addr_mode, &ops);
        let v = self.get_src_value(&addr_mode, ea.addr);

        self.set_nz_flags(FLAG_SIGN | FLAG_ZERO, v);
        self.write_register(src_reg, v);

        ea.clk_count
    }

    fn op_store(&mut self, addr_mode: &AddrMode, dst_reg: &RegMod) -> u64 {
        // let ops = self.fetch_op(addr_mode);
        // let ea: EAResult = self.calc_eff_addr(&InsMemAccess::Write, addr_mode, &ops);
        let v = self.read_register(dst_reg);
        self.mem.write_byte(ea.addr, v);
        ea.clk_count
    }

    fn op_tx(&mut self, src_reg: &RegMod, dst_reg: &RegMod) -> u64 {
        let v = self.read_register(src_reg);
        self.write_register(dst_reg, v);
        self.set_nz_flags(FLAG_SIGN | FLAG_ZERO, v);
        2
    }

    fn op_or(&mut self, addr_mode: &AddrMode) -> u64 {
        // let ops = self.fetch_op(addr_mode);
        // let ea: EAResult = self.calc_eff_addr(&InsMemAccess::ReadWrite, addr_mode, &ops);
        // let v = self.get_src_value(&addr_mode, ea.addr);
        self.mem.write_byte(ea.addr, v | self.regs.A);
        self.set_nz_flags(FLAG_SIGN | FLAG_ZERO, v);
        ea.clk_count
    }

    fn op_and(&mut self, addr_mode: &AddrMode) -> u64 {
        // let ops = self.fetch_op(addr_mode);
        // let ea: EAResult = self.calc_eff_addr(&InsMemAccess::ReadWrite, addr_mode, &ops);
        // let v = self.get_src_value(&addr_mode, ea.addr);
        self.mem.write_byte(ea.addr, v & self.regs.A);
        self.set_nz_flags(FLAG_SIGN | FLAG_ZERO, v);
        ea.clk_count
    }

    fn op_xor(&mut self, addr_mode: &AddrMode) -> u64 {
        // let ops = self.fetch_op(addr_mode);
        // let ea: EAResult = self.calc_eff_addr(&InsMemAccess::ReadWrite, addr_mode, &ops);
        // let v = self.get_src_value(&addr_mode, ea.addr);
        self.mem.write_byte(ea.addr, v ^ self.regs.A);
        self.set_nz_flags(FLAG_SIGN | FLAG_ZERO, v);
        ea.clk_count
    }

    fn op_adc(&mut self, addr_mode: &AddrMode) -> u64 {
        // let ops = self.fetch_op(addr_mode);
        // let ea: EAResult = self.calc_eff_addr(&InsMemAccess::Read, addr_mode, &ops);
        let v = self.get_src_value(&addr_mode, ea.addr);
        
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

    fn op_sbc(&mut self, addr_mode: &AddrMode) -> u64 {
        // let ops = self.fetch_op(addr_mode);
        // let ea: EAResult = self.calc_eff_addr(&InsMemAccess::Read, addr_mode, &ops);
        let v = self.get_src_value(&addr_mode, ea.addr);
        
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

    fn op_cmp(&mut self, addr_mode: &AddrMode, src_reg: &RegMod) -> u64 {
        // let ops = self.fetch_op(addr_mode);
        // let ea: EAResult = self.calc_eff_addr(&InsMemAccess::Read, addr_mode, &ops);
        let v = self.get_src_value(&addr_mode, ea.addr);
        
        let cmp_res: u32 = self.read_register(src_reg) as u32 - v as u32;
        if cmp_res < 0x100 {
            self.set_c_flag(true);
        }

        self.set_nz_flags(FLAG_SIGN | FLAG_ZERO, cmp_res as u8);
        ea.clk_count
    }

    fn op_dec(&mut self, addr_mode: &AddrMode, dst_reg: &RegMod) -> u64 {
        //handle DEX/DEY
        let v: u8;
        let clk: u64;

        if *dst_reg != RegMod::None {
            v = self.read_register(dst_reg);
            self.write_register(dst_reg, v.wrapping_sub(1));
            clk = 2; // two clocks
        } 
        else {
            let ops = self.fetch_op(addr_mode);
            // let ea: EAResult = self.calc_eff_addr(&InsMemAccess::ReadWrite, addr_mode, &ops);
            v = self.get_src_value(&addr_mode, ea.addr);
            self.mem.write_byte(ea.addr, v.wrapping_sub(1));
            clk = ea.clk_count;
        }        
       
        self.set_nz_flags(FLAG_SIGN | FLAG_ZERO, v);
        clk
    }

    fn op_inc(&mut self, addr_mode: &AddrMode, dst_reg: &RegMod) -> u64 {
        // handle INX/INY

        let v: u8;
        let clk: u64;

        if *dst_reg != RegMod::None {
            v = self.read_register(dst_reg);
            self.write_register(dst_reg, v.wrapping_add(1));
            clk = 2; // two clocks
        } 
        else {
            let ops = self.fetch_op(addr_mode);
            // let ea: EAResult = self.calc_eff_addr(&InsMemAccess::ReadWrite, addr_mode, &ops);
            v = self.get_src_value(&addr_mode, ea.addr);
            self.mem.write_byte(ea.addr, v.wrapping_add(1));
            clk = ea.clk_count;
        }        
       
        self.set_nz_flags(FLAG_SIGN | FLAG_ZERO, v);
        clk
    }

    fn op_shift(&mut self, addr_mode: &AddrMode, left: u8) -> u64 {
        let ops = self.fetch_op(addr_mode);
        // let ea: EAResult = self.calc_eff_addr(&InsMemAccess::ReadWrite, addr_mode, &ops);
        let mut v = self.get_src_value(&addr_mode, ea.addr);
        
        if left > 0 {  
            // ASL
            self.set_c_flag(v & 0x80 == 0x80);
            v = v << 1;
        } 
        else {
            // LSR
            
        }
       
        self.set_nz_flags(FLAG_SIGN | FLAG_ZERO, v);

        match *addr_mode {
            AddrMode::Acc => self.regs.A = v,
            _ => self.mem.write_byte(ea.addr, v)
        } 

        ea.clk_count
    }

    fn op_rot(&mut self, addr_mode: &AddrMode, left: u8) -> u64 {
        0
    }

    fn op_pull(&mut self, dst_reg: &RegMod) -> u64 {
        0
        
    }

    fn op_push(&mut self, src_reg: &RegMod) -> u64 {
        0
    }

    fn op_jump(&mut self, test_flag: u8, branch_if: u8) -> u64 {
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

    fn op_jmp(&mut self, addr_mode: &AddrMode) -> u64 {
        let ops = self.fetch_op(addr_mode);
        //let ea: EAResult = self.calc_eff_addr(&InsMemAccess::Jump, addr_mode, &ops);
        self.regs.PC = ea.addr;
        ea.clk_count
    }

    fn op_bit(&mut self, addrm: &AddrMode) -> u64 {
        0
    }

    fn op_setfl(&mut self, dst_flag: u8, f: u8) -> u64 {
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
    */
}
