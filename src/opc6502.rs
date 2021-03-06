/*----------------------------------------------------------------------------- 
   
This file is part of Core65X2 

Copyright (c) 2017 Hernán Di Pietro

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

-----------------------------------------------------------------------------*/

use AddrMode;

#[derive(PartialEq)]
pub enum Instr {
    INVALID, BRK, ORA, ASL, PHP, PLA, LDA, LDX, LDY, STA, STX, STY,
    CMP, CPX, CPY, DEC, DEX, DEY, INC, INX, INY, TSX, TXS, TAX, TXA,
    TYA, TAY, PLP, PHA, JMP, RTS, RTI, BNE, BCC, AND, EOR, BIT, SEC,
    CLI, CLC, SEI, BPL, ROL, ROR, LSR, SBC, ADC, BMI, BVC, CLD, BVS,
    BCS, CLV, NOP, BEQ, SED, JSR
}
pub struct Opcode {
    pub ins:    Instr,
    pub name:   &'static str,
    pub addr_m: AddrMode
}


macro_rules! Invalid_Opcode {
    () => {
       Opcode { ins: Instr::INVALID, name: "???", addr_m: AddrMode::Impl } 
    };
}

macro_rules! define_opcode {
    ($name:ident, $addrm:expr) => {
        Opcode { ins: Instr::$name, name: stringify!($name), addr_m: $addrm }
    }
}

/* 6502 Documented Opcodes */

pub const OPCODE_TABLE: &'static [Opcode; 256] = &[  
            /* 0x00 */ define_opcode!(BRK, AddrMode::Impl),
            /* 0x01 */ define_opcode!(ORA,  AddrMode::ZPIndX ),
            /* 0x02 */ Invalid_Opcode!(),
            /* 0x03 */ Invalid_Opcode!(),
            /* 0x04 */ Invalid_Opcode!(),
            /* 0x05 */ define_opcode!(ORA,  AddrMode::ZP     ),
            /* 0x06 */ define_opcode!(ASL,  AddrMode::ZP     ),
            /* 0x07 */ Invalid_Opcode!(),
            /* 0x08 */ define_opcode!(PHP,  AddrMode::Impl   ),
            /* 0x09 */ define_opcode!(ORA,  AddrMode::Imm    ),
            /* 0x0A */ define_opcode!(ASL,  AddrMode::Acc    ),
            /* 0x0B */ Invalid_Opcode!(),
            /* 0x0C */ Invalid_Opcode!(),
            /* 0x0D */ define_opcode!(ORA,  AddrMode::Abs    ),
            /* 0x0E */ define_opcode!(ASL,  AddrMode::Abs    ),
            /* 0x0F */ Invalid_Opcode!(),
            /* 0x10 */ define_opcode!(BPL,  AddrMode::Rel    ),
            /* 0x11 */ define_opcode!(ORA,  AddrMode::ZPIndY ),
            /* 0x12 */ Invalid_Opcode!(),
            /* 0x13 */ Invalid_Opcode!(),
            /* 0x14 */ Invalid_Opcode!(),
            /* 0x15 */ define_opcode!(ORA,  AddrMode::ZPX    ),
            /* 0x16 */ define_opcode!(ASL,  AddrMode::ZPX    ),
            /* 0x17 */ Invalid_Opcode!(),
            /* 0x18 */ define_opcode!(CLC,  AddrMode::Impl   ),
            /* 0x19 */ define_opcode!(ORA,  AddrMode::AbsY   ),
            /* 0x1A */ Invalid_Opcode!(),
            /* 0x1B */ Invalid_Opcode!(),
            /* 0x1C */ Invalid_Opcode!(),
            /* 0x1D */ define_opcode!(ORA,  AddrMode::AbsX   ),
            /* 0x1E */ define_opcode!(ASL,  AddrMode::AbsX   ),
            /* 0x1F */ Invalid_Opcode!(),
            /* 0x20 */ define_opcode!(JSR,  AddrMode::Abs    ),
            /* 0x21 */ define_opcode!(AND,  AddrMode::ZPIndX ),
            /* 0x22 */ Invalid_Opcode!(),
            /* 0x23 */ Invalid_Opcode!(),
            /* 0x24 */ define_opcode!(BIT,  AddrMode::ZP     ),
            /* 0x25 */ define_opcode!(AND,  AddrMode::ZP     ),
            /* 0x26 */ define_opcode!(ROL,  AddrMode::ZP     ),
            /* 0x27 */ Invalid_Opcode!(),
            /* 0x28 */ define_opcode!(PLP,  AddrMode::Impl   ),
            /* 0x29 */ define_opcode!(AND,  AddrMode::Imm    ),
            /* 0x2A */ define_opcode!(ROL,  AddrMode::Acc    ),
            /* 0x2B */ Invalid_Opcode!(),
            /* 0x2C */ define_opcode!(BIT,  AddrMode::Abs    ),
            /* 0x2D */ define_opcode!(AND,  AddrMode::Abs    ),
            /* 0x2E */ define_opcode!(ROL,  AddrMode::Abs    ),
            /* 0x2F */ Invalid_Opcode!(),
            /* 0x30 */ define_opcode!(BMI,  AddrMode::Rel    ),
            /* 0x31 */ define_opcode!(AND,  AddrMode::ZPIndY ),
            /* 0x32 */ Invalid_Opcode!(),
            /* 0x33 */ Invalid_Opcode!(),
            /* 0x34 */ Invalid_Opcode!(),
            /* 0x35 */ define_opcode!(AND,  AddrMode::ZPX    ),
            /* 0x36 */ define_opcode!(ROL,  AddrMode::ZPX    ),
            /* 0x37 */ Invalid_Opcode!(),
            /* 0x38 */ define_opcode!(SEC,  AddrMode::Impl   ),
            /* 0x39 */ define_opcode!(AND,  AddrMode::AbsY   ),
            /* 0x3A */ Invalid_Opcode!(),
            /* 0x3B */ Invalid_Opcode!(),
            /* 0x3C */ Invalid_Opcode!(),
            /* 0x3D */ define_opcode!(AND,  AddrMode::AbsX   ),
            /* 0x3E */ define_opcode!(ROL,  AddrMode::AbsX   ),
            /* 0x3F */ Invalid_Opcode!(),
            /* 0x40 */ define_opcode!(RTI,  AddrMode::Impl   ),
            /* 0x41 */ define_opcode!(EOR,  AddrMode::ZPIndX ),
            /* 0x42 */ Invalid_Opcode!(),
            /* 0x43 */ Invalid_Opcode!(),
            /* 0x44 */ Invalid_Opcode!(),
            /* 0x45 */ define_opcode!(EOR,  AddrMode::ZP     ),
            /* 0x46 */ define_opcode!(LSR,  AddrMode::ZP     ),
            /* 0x47 */ Invalid_Opcode!(),
            /* 0x48 */ define_opcode!(PHA,  AddrMode::Impl   ),
            /* 0x49 */ define_opcode!(EOR,  AddrMode::Imm    ),
            /* 0x4A */ define_opcode!(LSR,  AddrMode::Acc    ),
            /* 0x4B */ Invalid_Opcode!(),
            /* 0x4C */ define_opcode!(JMP,  AddrMode::Abs    ),
            /* 0x4D */ define_opcode!(EOR,  AddrMode::Abs    ),
            /* 0x4E */ define_opcode!(LSR,  AddrMode::Abs    ),
            /* 0x4F */ Invalid_Opcode!(),
            /* 0x50 */ define_opcode!(BVC,  AddrMode::Rel    ),
            /* 0x51 */ define_opcode!(EOR,  AddrMode::ZPIndY ),
            /* 0x52 */ Invalid_Opcode!(),
            /* 0x53 */ Invalid_Opcode!(),
            /* 0x54 */ Invalid_Opcode!(),
            /* 0x55 */ define_opcode!(EOR,  AddrMode::ZPX    ),
            /* 0x56 */ define_opcode!(LSR,  AddrMode::ZPX    ),
            /* 0x57 */ Invalid_Opcode!(),
            /* 0x58 */ define_opcode!(CLI,  AddrMode::Impl   ),
            /* 0x59 */ define_opcode!(EOR,  AddrMode::AbsY   ),
            /* 0x5A */ Invalid_Opcode!(),
            /* 0x5B */ Invalid_Opcode!(),
            /* 0x5C */ Invalid_Opcode!(),
            /* 0x5D */ define_opcode!(EOR,  AddrMode::AbsX   ),
            /* 0x5E */ define_opcode!(LSR,  AddrMode::AbsX   ),
            /* 0x5F */ Invalid_Opcode!(),
            /* 0x60 */ define_opcode!(RTS,  AddrMode::Impl   ),
            /* 0x61 */ define_opcode!(ADC,  AddrMode::ZPIndX ),
            /* 0x62 */ Invalid_Opcode!(),
            /* 0x63 */ Invalid_Opcode!(),
            /* 0x64 */ Invalid_Opcode!(),
            /* 0x65 */ define_opcode!(ADC,  AddrMode::ZP     ),
            /* 0x66 */ define_opcode!(ROR,  AddrMode::ZP     ),
            /* 0x67 */ Invalid_Opcode!(),
            /* 0x68 */ define_opcode!(PLA,  AddrMode::Impl   ),
            /* 0x69 */ define_opcode!(ADC,  AddrMode::Imm    ),
            /* 0x6A */ define_opcode!(ROR,  AddrMode::Acc    ),
            /* 0x6B */ Invalid_Opcode!(),
            /* 0x6C */ define_opcode!(JMP,  AddrMode::Ind    ),
            /* 0x6D */ define_opcode!(ADC,  AddrMode::Abs    ),
            /* 0x6E */ define_opcode!(ROR,  AddrMode::Abs    ),
            /* 0x6F */ Invalid_Opcode!(),
            /* 0x70 */ define_opcode!(BVS,  AddrMode::Rel    ),
            /* 0x71 */ define_opcode!(ADC,  AddrMode::ZPIndY ),
            /* 0x72 */ Invalid_Opcode!(),
            /* 0x73 */ Invalid_Opcode!(),
            /* 0x74 */ Invalid_Opcode!(),
            /* 0x75 */ define_opcode!(ADC,  AddrMode::ZPX    ),
            /* 0x76 */ define_opcode!(ROR,  AddrMode::ZPX    ),
            /* 0x77 */ Invalid_Opcode!(),
            /* 0x78 */ define_opcode!(SEI,  AddrMode::Impl    ),
            /* 0x79 */ define_opcode!(ADC,  AddrMode::AbsY   ),
            /* 0x7A */ Invalid_Opcode!(),
            /* 0x7B */ Invalid_Opcode!(),
            /* 0x7C */ Invalid_Opcode!(),
            /* 0x7D */ define_opcode!(ADC,  AddrMode::AbsX   ),
            /* 0x7E */ define_opcode!(ROR,  AddrMode::AbsX   ),
            /* 0x7F */ Invalid_Opcode!(),
            /* 0x80 */ Invalid_Opcode!(),
            /* 0x81 */ define_opcode!(STA,  AddrMode::ZPIndX ),
            /* 0x82 */ Invalid_Opcode!(),
            /* 0x83 */ Invalid_Opcode!(),
            /* 0x84 */ define_opcode!(STY,  AddrMode::ZP     ),
            /* 0x85 */ define_opcode!(STA,  AddrMode::ZP     ),
            /* 0x86 */ define_opcode!(STX,  AddrMode::ZP     ),
            /* 0x87 */ Invalid_Opcode!(),
            /* 0x88 */ define_opcode!(DEY,  AddrMode::Impl   ),
            /* 0x89 */ Invalid_Opcode!(),
            /* 0x8A */ define_opcode!(TXA,  AddrMode::Impl  ),
            /* 0x8B */ Invalid_Opcode!(),
            /* 0x8C */ define_opcode!(STY,  AddrMode::Abs    ),
            /* 0x8D */ define_opcode!(STA,  AddrMode::Abs    ),
            /* 0x8E */ define_opcode!(STX,  AddrMode::Abs    ),
            /* 0x8F */ Invalid_Opcode!(),
            /* 0x90 */ define_opcode!(BCC,  AddrMode::Rel    ),
            /* 0x91 */ define_opcode!(STA,  AddrMode::ZPIndY ),
            /* 0x92 */ Invalid_Opcode!(),
            /* 0x93 */ Invalid_Opcode!(),
            /* 0x94 */ define_opcode!(STY,  AddrMode::ZPX    ),
            /* 0x95 */ define_opcode!(STA,  AddrMode::ZPX    ),
            /* 0x96 */ define_opcode!(STX,  AddrMode::ZPY    ),
            /* 0x97 */ Invalid_Opcode!(),
            /* 0x98 */ define_opcode!(TYA,  AddrMode::Impl   ),
            /* 0x99 */ define_opcode!(STA,  AddrMode::AbsY   ),
            /* 0x9A */ define_opcode!(TXS,  AddrMode::Impl   ),
            /* 0x9B */ Invalid_Opcode!(),
            /* 0x9C */ Invalid_Opcode!(),
            /* 0x9D */ define_opcode!(STA,  AddrMode::AbsX   ),
            /* 0x9E */ Invalid_Opcode!(),
            /* 0x9F */ Invalid_Opcode!(),
            /* 0xA0 */ define_opcode!(LDY,  AddrMode::Imm    ),
            /* 0xA1 */ define_opcode!(LDA,  AddrMode::ZPIndX ),
            /* 0xA2 */ define_opcode!(LDX,  AddrMode::Imm    ),
            /* 0xA3 */ Invalid_Opcode!(),
            /* 0xA4 */ define_opcode!(LDY,  AddrMode::ZP     ),
            /* 0xA5 */ define_opcode!(LDA,  AddrMode::ZP     ),
            /* 0xA6 */ define_opcode!(LDX,  AddrMode::ZP     ),
            /* 0xA7 */ Invalid_Opcode!(),
            /* 0xA8 */ define_opcode!(TAY,  AddrMode::Impl  ),
            /* 0xA9 */ define_opcode!(LDA,  AddrMode::Imm    ),
            /* 0xAA */ define_opcode!(TAX,  AddrMode::Impl  ),
            /* 0xAB */ Invalid_Opcode!(),
            /* 0xAC */ define_opcode!(LDY,  AddrMode::Abs    ),
            /* 0xAD */ define_opcode!(LDA,  AddrMode::Abs    ),
            /* 0xAE */ define_opcode!(LDX,  AddrMode::Abs    ),
            /* 0xAF */ Invalid_Opcode!(),
            /* 0xB0 */ define_opcode!(BCS,  AddrMode::Rel    ),
            /* 0xB1 */ define_opcode!(LDA,  AddrMode::ZPIndY ),
            /* 0xB2 */ Invalid_Opcode!(),
            /* 0xB3 */ Invalid_Opcode!(),
            /* 0xB4 */ define_opcode!(LDY,  AddrMode::ZPX    ),
            /* 0xB5 */ define_opcode!(LDA,  AddrMode::ZPX    ),
            /* 0xB6 */ define_opcode!(LDX,  AddrMode::ZPY    ),
            /* 0xB7 */ Invalid_Opcode!(),
            /* 0xB8 */ define_opcode!(CLV,  AddrMode::Impl   ),
            /* 0xB9 */ define_opcode!(LDA,  AddrMode::AbsY   ),
            /* 0xBA */ define_opcode!(TSX,  AddrMode::Impl   ),
            /* 0xBB */ Invalid_Opcode!(),
            /* 0xBC */ define_opcode!(LDY,  AddrMode::AbsX   ),
            /* 0xBD */ define_opcode!(LDA,  AddrMode::AbsX   ),
            /* 0xBE */ define_opcode!(LDX,  AddrMode::AbsY   ),
            /* 0xBF */ Invalid_Opcode!(),
            /* 0xC0 */ define_opcode!(CPY,  AddrMode::Imm    ),
            /* 0xC1 */ define_opcode!(CMP,  AddrMode::ZPIndX ),
            /* 0xC2 */ Invalid_Opcode!(),
            /* 0xC3 */ Invalid_Opcode!(),
            /* 0xC4 */ define_opcode!(CPY,  AddrMode::ZP     ),
            /* 0xC5 */ define_opcode!(CMP,  AddrMode::ZP     ),
            /* 0xC6 */ define_opcode!(DEC,  AddrMode::ZP     ),
            /* 0xC7 */ Invalid_Opcode!(),
            /* 0xC8 */ define_opcode!(INY,  AddrMode::Impl   ),
            /* 0xC9 */ define_opcode!(CMP,  AddrMode::Imm    ),
            /* 0xCA */ define_opcode!(DEX,  AddrMode::Impl   ),
            /* 0xCB */ Invalid_Opcode!(),
            /* 0xCC */ define_opcode!(CPY,  AddrMode::Abs    ),
            /* 0xCD */ define_opcode!(CMP,  AddrMode::Abs    ),
            /* 0xCE */ define_opcode!(DEC,  AddrMode::Abs    ),
            /* 0xCF */ Invalid_Opcode!(),
            /* 0xD0 */ define_opcode!(BNE,  AddrMode::Rel    ),
            /* 0xD1 */ define_opcode!(CMP,  AddrMode::ZPIndY ),
            /* 0xD2 */ Invalid_Opcode!(),
            /* 0xD3 */ Invalid_Opcode!(),
            /* 0xD4 */ Invalid_Opcode!(),
            /* 0xD5 */ define_opcode!(CMP,  AddrMode::ZPX    ),
            /* 0xD6 */ define_opcode!(DEC,  AddrMode::ZPX    ),
            /* 0xD7 */ Invalid_Opcode!(),
            /* 0xD8 */ define_opcode!(CLD,  AddrMode::Impl   ),
            /* 0xD9 */ define_opcode!(CMP,  AddrMode::AbsY   ),
            /* 0xDA */ Invalid_Opcode!(),
            /* 0xDB */ Invalid_Opcode!(),
            /* 0xDC */ Invalid_Opcode!(),
            /* 0xDD */ define_opcode!(CMP,  AddrMode::AbsX   ),
            /* 0xDE */ define_opcode!(DEC,  AddrMode::AbsX   ),
            /* 0xDF */ Invalid_Opcode!(),
            /* 0xE0 */ define_opcode!(CPX,  AddrMode::Imm    ),
            /* 0xE1 */ define_opcode!(SBC,  AddrMode::ZPIndX ),
            /* 0xE2 */ Invalid_Opcode!(),
            /* 0xE3 */ Invalid_Opcode!(),
            /* 0xE4 */ define_opcode!(CPX,  AddrMode::ZP     ),
            /* 0xE5 */ define_opcode!(SBC,  AddrMode::ZP     ),
            /* 0xE6 */ define_opcode!(INC,  AddrMode::ZP     ),
            /* 0xE7 */ Invalid_Opcode!(),
            /* 0xE8 */ define_opcode!(INX,  AddrMode::Impl   ),
            /* 0xE9 */ define_opcode!(SBC,  AddrMode::Imm    ),
            /* 0xEA */ define_opcode!(NOP,  AddrMode::Impl   ),
            /* 0xEB */ Invalid_Opcode!(),
            /* 0xEC */ define_opcode!(CPX,  AddrMode::Abs    ),
            /* 0xED */ define_opcode!(SBC,  AddrMode::Abs    ),
            /* 0xEE */ define_opcode!(INC,  AddrMode::Abs    ),
            /* 0xEF */ Invalid_Opcode!(),
            /* 0xF0 */ define_opcode!(BEQ,  AddrMode::Rel    ),
            /* 0xF1 */ define_opcode!(SBC,  AddrMode::ZPIndY ),
            /* 0xF2 */ Invalid_Opcode!(),
            /* 0xF3 */ Invalid_Opcode!(),
            /* 0xF4 */ Invalid_Opcode!(),
            /* 0xF5 */ define_opcode!(SBC,  AddrMode::ZPX    ),
            /* 0xF6 */ define_opcode!(INC,  AddrMode::ZPX    ),
            /* 0xF7 */ Invalid_Opcode!(),
            /* 0xF8 */ define_opcode!(SED,  AddrMode::Impl   ),
            /* 0xF9 */ define_opcode!(SBC,  AddrMode::AbsY   ),
            /* 0xFA */ Invalid_Opcode!(),
            /* 0xFB */ Invalid_Opcode!(),
            /* 0xFC */ Invalid_Opcode!(),
            /* 0xFD */ define_opcode!(SBC,  AddrMode::AbsX   ),
            /* 0xFE */ define_opcode!(INC,  AddrMode::AbsX   ),
            /* 0xFF */ Invalid_Opcode!(),
];

/* THANKS TO: Fake6502 (c)2011 Mike Chambers (miker00lz@gmail.com)     */
pub const CLK_TABLE: [u64; 256] = [
/*        |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  A  |  B  |  C  |  D  |  E  |  F  |     */
/* 0 */      7,    6,    2,    8,    3,    3,    5,    5,    3,    2,    2,    2,    4,    4,    6,    6,  /* 0 */
/* 1 */      2,    5,    2,    8,    4,    4,    6,    6,    2,    4,    2,    7,    4,    4,    7,    7,  /* 1 */
/* 2 */      6,    6,    2,    8,    3,    3,    5,    5,    4,    2,    2,    2,    4,    4,    6,    6,  /* 2 */
/* 3 */      2,    5,    2,    8,    4,    4,    6,    6,    2,    4,    2,    7,    4,    4,    7,    7,  /* 3 */
/* 4 */      6,    6,    2,    8,    3,    3,    5,    5,    3,    2,    2,    2,    3,    4,    6,    6,  /* 4 */
/* 5 */      2,    5,    2,    8,    4,    4,    6,    6,    2,    4,    2,    7,    4,    4,    7,    7,  /* 5 */
/* 6 */      6,    6,    2,    8,    3,    3,    5,    5,    4,    2,    2,    2,    5,    4,    6,    6,  /* 6 */
/* 7 */      2,    5,    2,    8,    4,    4,    6,    6,    2,    4,    2,    7,    4,    4,    7,    7,  /* 7 */
/* 8 */      2,    6,    2,    6,    3,    3,    3,    3,    2,    2,    2,    2,    4,    4,    4,    4,  /* 8 */
/* 9 */      2,    6,    2,    6,    4,    4,    4,    4,    2,    5,    2,    5,    5,    5,    5,    5,  /* 9 */
/* A */      2,    6,    2,    6,    3,    3,    3,    3,    2,    2,    2,    2,    4,    4,    4,    4,  /* A */
/* B */      2,    5,    2,    5,    4,    4,    4,    4,    2,    4,    2,    4,    4,    4,    4,    4,  /* B */
/* C */      2,    6,    2,    8,    3,    3,    5,    5,    2,    2,    2,    2,    4,    4,    6,    6,  /* C */
/* D */      2,    5,    2,    8,    4,    4,    6,    6,    2,    4,    2,    7,    4,    4,    7,    7,  /* D */
/* E */      2,    6,    2,    8,    3,    3,    5,    5,    2,    2,    2,    2,    4,    4,    6,    6,  /* E */
/* F */      2,    5,    2,    8,    4,    4,    6,    6,    2,    4,    2,    7,    4,    4,    7,    7   /* F */
];
