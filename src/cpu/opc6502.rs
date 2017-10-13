use cpu::AddrMode;
use cpu::RegMod;
use cpu::{ FLAG_CARRY, FLAG_DEC, FLAG_INTR, FLAG_OF, FLAG_SIGN, FLAG_ZERO};

enum InsGrp {
    Load, Store, Tx_from_A, Tx_from_X, Tx_from_Y, Tx_from_SP,
    Push, Pull,  Or, And, Xor, Adc, Sbc, Cmp, Dec, Inc, Shift, Rot,
    Jump,  Jsr, Rti, Rts, CJump, Bit, Brk, Flag, Nop,
    Invalid
}

/*

    ADC, AND, ASL, BCC, BCS, BEQ, BIT, BRK, BMI, BNE, BPL, BVC, 
    BVS, CLC, CLD, CLI, CLV, CMP, CPX, CPY, DEC, DEX, DEY, EOR,
    INC, INX, INY, JMP, JSR, LDA, LDX, LDY, LSR, NOP, ORA, PHA,
    PHP, PLP, PLA, ROL, ROR, RTI, RTS, SBC, SEC, SED, SEI, STA,
    STX, STY, TAX, TAY, TSX, TXA, TYA, TXS
}
*/

type FlagMod = u8;

struct Opcode {
    addr_m: AddrMode,
    reg_m:  RegMod,
    flag_m: FlagMod,
    args:   u8,
    ins:    InsGrp,
}

macro_rules! Invalid_Opcode {
    () => {
       Opcode { addr_m: AddrMode::Impl, reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Invalid } 
    };
}

/* 6502 Documented Opcodes */

const opcode_table: &'static [Opcode; 256] = &[  
            /* 0x00 */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Brk },
            /* 0x01 */ Opcode {addr_m: AddrMode::ZPIndX,    reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Or },
            /* 0x02 */ Invalid_Opcode!(),
            /* 0x03 */ Invalid_Opcode!(),
            /* 0x04 */ Invalid_Opcode!(),
            /* 0x05 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Or },
            /* 0x06 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Shift },
            /* 0x07 */ Invalid_Opcode!(),
            /* 0x08 */ Opcode { addr_m: AddrMode::Impl,     reg_m: RegMod::SR,flag_m: 0, args: 0, ins: InsGrp::Push },
            /* 0x09 */ Opcode {addr_m: AddrMode::Imm,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Or },
            /* 0x0A */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Shift },
            /* 0x0B */ Invalid_Opcode!(),
            /* 0x0C */ Invalid_Opcode!(),
            /* 0x0D */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Or },
            /* 0x0E */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Shift },
            /* 0x0F */ Invalid_Opcode!(),
            /* 0x10 */ Opcode {addr_m: AddrMode::Rel,       reg_m: RegMod::None, flag_m: FLAG_SIGN, args: 0, ins: InsGrp::CJump },
            /* 0x11 */ Opcode {addr_m: AddrMode::ZPIndY,    reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Or },
            /* 0x12 */ Invalid_Opcode!(),
            /* 0x13 */ Invalid_Opcode!(),
            /* 0x14 */ Invalid_Opcode!(),
            /* 0x15 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Or },
            /* 0x16 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Shift },
            /* 0x17 */ Invalid_Opcode!(),
            /* 0x18 */ Opcode { addr_m: AddrMode::Impl,       reg_m: RegMod::None, flag_m: FLAG_CARRY, args: 0, ins: InsGrp::Flag },
            /* 0x19 */ Opcode {addr_m: AddrMode::AbsY,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Or },
            /* 0x1A */ Invalid_Opcode!(),
            /* 0x1B */ Invalid_Opcode!(),
            /* 0x1C */ Invalid_Opcode!(),
            /* 0x1D */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Or },
            /* 0x1E */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Shift },
            /* 0x1F */ Invalid_Opcode!(),
            /* 0x20 */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Jsr },
            /* 0x21 */ Opcode {addr_m: AddrMode::ZPIndX,    reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::And },
            /* 0x22 */ Invalid_Opcode!(),
            /* 0x23 */ Invalid_Opcode!(),
            /* 0x24 */ Opcode { addr_m: AddrMode::ZP ,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Bit },
            /* 0x25 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::And },
            /* 0x26 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Rot },
            /* 0x27 */ Invalid_Opcode!(),
            /* 0x28 */ Opcode { addr_m: AddrMode::Impl,     reg_m: RegMod::SR,flag_m: 0, args: 0, ins: InsGrp::Pull },
            /* 0x29 */ Opcode {addr_m: AddrMode::Imm,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::And },
            /* 0x2A */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Rot },
            /* 0x2B */ Invalid_Opcode!(),
            /* 0x2C */ Opcode { addr_m: AddrMode::Abs,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Bit },
            /* 0x2D */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::And },
            /* 0x2E */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Rot },
            /* 0x2F */ Invalid_Opcode!(),
            /* 0x30 */ Opcode {addr_m: AddrMode::Rel,       reg_m: RegMod::None, flag_m: FLAG_SIGN, args: 1, ins: InsGrp::CJump },
            /* 0x31 */ Opcode {addr_m: AddrMode::ZPIndY,    reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::And },
            /* 0x32 */ Invalid_Opcode!(),
            /* 0x33 */ Invalid_Opcode!(),
            /* 0x34 */ Invalid_Opcode!(),
            /* 0x35 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::And },
            /* 0x36 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Rot },
            /* 0x37 */ Invalid_Opcode!(),
            /* 0x38 */ Opcode { addr_m: AddrMode::Impl,       reg_m: RegMod::None, flag_m: FLAG_CARRY, args: 1, ins: InsGrp::Flag },
            /* 0x39 */ Opcode {addr_m: AddrMode::AbsY,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::And },
            /* 0x3A */ Invalid_Opcode!(),
            /* 0x3B */ Invalid_Opcode!(),
            /* 0x3C */ Invalid_Opcode!(),
            /* 0x3D */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::And },
            /* 0x3E */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Rot },
            /* 0x3F */ Invalid_Opcode!(),
            /* 0x40 */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Rti },
            /* 0x41 */ Opcode {addr_m: AddrMode::ZPIndX,    reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Xor },
            /* 0x42 */ Invalid_Opcode!(),
            /* 0x43 */ Invalid_Opcode!(),
            /* 0x44 */ Invalid_Opcode!(),
            /* 0x45 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Xor },
            /* 0x46 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::None, flag_m: 0, args: 1, ins: InsGrp::Shift },
            /* 0x47 */ Invalid_Opcode!(),
            /* 0x48 */ Opcode { addr_m: AddrMode::Impl,     reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Push },
            /* 0x49 */ Opcode {addr_m: AddrMode::Imm,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Xor },
            /* 0x4A */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::None, flag_m: 0, args: 1, ins: InsGrp::Shift },
            /* 0x4B */ Invalid_Opcode!(),
            /* 0x4C */ Opcode { addr_m: AddrMode::Abs ,     reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Jump },
            /* 0x4D */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Xor },
            /* 0x4E */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::None, flag_m: 0, args: 1, ins: InsGrp::Shift },
            /* 0x4F */ Invalid_Opcode!(),
            /* 0x50 */ Opcode {addr_m: AddrMode::Rel,       reg_m: RegMod::None, flag_m: FLAG_OF,   args: 0, ins: InsGrp::CJump },
            /* 0x51 */ Opcode {addr_m: AddrMode::ZPIndY,    reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Xor },
            /* 0x52 */ Invalid_Opcode!(),
            /* 0x53 */ Invalid_Opcode!(),
            /* 0x54 */ Invalid_Opcode!(),
            /* 0x55 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Xor },
            /* 0x56 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::None, flag_m: 0, args: 1, ins: InsGrp::Shift },
            /* 0x57 */ Invalid_Opcode!(),
            /* 0x58 */ Opcode { addr_m: AddrMode::Impl,       reg_m: RegMod::None, flag_m: FLAG_INTR,  args: 0, ins: InsGrp::Flag },
            /* 0x59 */ Opcode {addr_m: AddrMode::AbsY,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Xor },
            /* 0x5A */ Invalid_Opcode!(),
            /* 0x5B */ Invalid_Opcode!(),
            /* 0x5C */ Invalid_Opcode!(),
            /* 0x5D */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Xor },
            /* 0x5E */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::None, flag_m: 0, args: 1, ins: InsGrp::Shift },
            /* 0x5F */ Invalid_Opcode!(),
            /* 0x60 */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Rts },
            /* 0x61 */ Opcode {addr_m: AddrMode::ZPIndX ,   reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Adc },
            /* 0x62 */ Invalid_Opcode!(),
            /* 0x63 */ Invalid_Opcode!(),
            /* 0x64 */ Invalid_Opcode!(),
            /* 0x65 */ Opcode {addr_m: AddrMode::ZP ,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Adc },
            /* 0x66 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::None, flag_m: 0, args: 1, ins: InsGrp::Rot },
            /* 0x67 */ Invalid_Opcode!(),
            /* 0x68 */ Opcode { addr_m: AddrMode::Impl,     reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Pull },
            /* 0x69 */ Opcode {addr_m: AddrMode::Imm,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Adc },
            /* 0x6A */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::None, flag_m: 0, args: 1, ins: InsGrp::Rot },
            /* 0x6B */ Invalid_Opcode!(),
            /* 0x6C */ Opcode { addr_m: AddrMode::Ind ,     reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Jump },
            /* 0x6D */ Opcode {addr_m: AddrMode::Abs ,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Adc },
            /* 0x6E */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::None, flag_m: 0, args: 1, ins: InsGrp::Rot },
            /* 0x6F */ Invalid_Opcode!(),
            /* 0x70 */ Opcode {addr_m: AddrMode::Rel,       reg_m: RegMod::None, flag_m: FLAG_OF,   args: 1, ins: InsGrp::CJump },
            /* 0x71 */ Opcode {addr_m: AddrMode::ZPIndY ,   reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Adc },
            /* 0x72 */ Invalid_Opcode!(),
            /* 0x73 */ Invalid_Opcode!(),
            /* 0x74 */ Invalid_Opcode!(),
            /* 0x75 */ Opcode {addr_m: AddrMode::ZPX ,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Adc },
            /* 0x76 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::None, flag_m: 0, args: 1, ins: InsGrp::Rot },
            /* 0x77 */ Invalid_Opcode!(),
            /* 0x78 */ Opcode { addr_m: AddrMode::Impl,       reg_m: RegMod::None, flag_m: FLAG_INTR,  args: 1, ins: InsGrp::Flag },
            /* 0x79 */ Opcode {addr_m: AddrMode::AbsY ,     reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Adc },
            /* 0x7A */ Invalid_Opcode!(),
            /* 0x7B */ Invalid_Opcode!(),
            /* 0x7C */ Invalid_Opcode!(),
            /* 0x7D */ Opcode {addr_m: AddrMode::AbsX ,     reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Adc },
            /* 0x7E */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::None, flag_m: 0, args: 1, ins: InsGrp::Rot },
            /* 0x7F */ Invalid_Opcode!(),
            /* 0x80 */ Invalid_Opcode!(),
            /* 0x81 */ Opcode {addr_m: AddrMode::ZPIndX,    reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x82 */ Invalid_Opcode!(),
            /* 0x83 */ Invalid_Opcode!(),
            /* 0x84 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x85 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x86 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x87 */ Invalid_Opcode!(),
            /* 0x88 */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Dec },
            /* 0x89 */ Invalid_Opcode!(),
            /* 0x8A */ Opcode { addr_m: AddrMode::Impl,     reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Tx_from_X },
            /* 0x8B */ Invalid_Opcode!(),
            /* 0x8C */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x8D */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x8E */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x8F */ Invalid_Opcode!(),
            /* 0x90 */ Opcode {addr_m: AddrMode::Rel,       reg_m: RegMod::None, flag_m: FLAG_CARRY,args: 0, ins: InsGrp::CJump },
            /* 0x91 */ Opcode {addr_m: AddrMode::ZPIndY,    reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x92 */ Invalid_Opcode!(),
            /* 0x93 */ Invalid_Opcode!(),
            /* 0x94 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x95 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x96 */ Opcode {addr_m: AddrMode::ZPY,       reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x97 */ Invalid_Opcode!(),
            /* 0x98 */ Opcode { addr_m: AddrMode::Impl,     reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Tx_from_Y },
            /* 0x99 */ Opcode {addr_m: AddrMode::AbsY,      reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x9A */ Opcode { addr_m: AddrMode::Impl,     reg_m: RegMod::SP,flag_m: 0, args: 0, ins: InsGrp::Tx_from_X },
            /* 0x9B */ Invalid_Opcode!(),
            /* 0x9C */ Invalid_Opcode!(),
            /* 0x9D */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x9E */ Invalid_Opcode!(),
            /* 0x9F */ Invalid_Opcode!(),
            /* 0xA0 */ Opcode {addr_m: AddrMode::Imm,       reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xA1 */ Opcode {addr_m: AddrMode::ZPIndX,    reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xA2 */ Opcode {addr_m: AddrMode::Imm,       reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xA3 */ Invalid_Opcode!(),
            /* 0xA4 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xA5 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xA6 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xA7 */ Invalid_Opcode!(),
            /* 0xA8 */ Opcode { addr_m: AddrMode::Impl,     reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Tx_from_A },
            /* 0xA9 */ Opcode {addr_m: AddrMode::Imm,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xAA */ Opcode { addr_m: AddrMode::Impl,     reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Tx_from_A },
            /* 0xAB */ Invalid_Opcode!(),
            /* 0xAC */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xAD */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xAE */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xAF */ Invalid_Opcode!(),
            /* 0xB0 */ Opcode {addr_m: AddrMode::Rel,       reg_m: RegMod::None, flag_m: FLAG_CARRY,args: 1, ins: InsGrp::CJump },
            /* 0xB1 */ Opcode {addr_m: AddrMode::ZPIndY,    reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xB2 */ Invalid_Opcode!(),
            /* 0xB3 */ Invalid_Opcode!(),
            /* 0xB4 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xB5 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xB6 */ Opcode {addr_m: AddrMode::ZPY,       reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xB7 */ Invalid_Opcode!(),
            /* 0xB8 */ Opcode { addr_m: AddrMode::Impl,       reg_m: RegMod::None, flag_m: FLAG_OF,    args: 0, ins: InsGrp::Flag },
            /* 0xB9 */ Opcode {addr_m: AddrMode::AbsY,      reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xBA */ Opcode { addr_m: AddrMode::Impl,     reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Tx_from_SP},
            /* 0xBB */ Invalid_Opcode!(),
            /* 0xBC */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xBD */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xBE */ Opcode {addr_m: AddrMode::AbsY,      reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xBF */ Invalid_Opcode!(),
            /* 0xC0 */ Opcode {addr_m: AddrMode::Imm,       reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xC1 */ Opcode {addr_m: AddrMode::ZPIndX,    reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xC2 */ Invalid_Opcode!(),
            /* 0xC3 */ Invalid_Opcode!(),
            /* 0xC4 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xC5 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xC6 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Dec },
            /* 0xC7 */ Invalid_Opcode!(),
            /* 0xC8 */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Inc },
            /* 0xC9 */ Opcode {addr_m: AddrMode::Imm,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xCA */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Dec },
            /* 0xCB */ Invalid_Opcode!(),
            /* 0xCC */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xCD */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xCE */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Dec },
            /* 0xCF */ Invalid_Opcode!(),
            /* 0xD0 */ Opcode {addr_m: AddrMode::Rel,       reg_m: RegMod::None, flag_m: FLAG_ZERO, args: 0, ins: InsGrp::CJump },
            /* 0xD1 */ Opcode {addr_m: AddrMode::ZPIndY,    reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xD2 */ Invalid_Opcode!(),
            /* 0xD3 */ Invalid_Opcode!(),
            /* 0xD4 */ Invalid_Opcode!(),
            /* 0xD5 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xD6 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Dec },
            /* 0xD7 */ Invalid_Opcode!(),
            /* 0xD8 */ Opcode { addr_m: AddrMode::Impl,       reg_m: RegMod::None, flag_m: FLAG_DEC,   args: 0, ins: InsGrp::Flag },
            /* 0xD9 */ Opcode {addr_m: AddrMode::AbsY,      reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xDA */ Invalid_Opcode!(),
            /* 0xDB */ Invalid_Opcode!(),
            /* 0xDC */ Invalid_Opcode!(),
            /* 0xDD */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xDE */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Dec },
            /* 0xDF */ Invalid_Opcode!(),
            /* 0xE0 */ Opcode {addr_m: AddrMode::Imm,       reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xE1 */ Opcode {addr_m: AddrMode::ZPIndX ,   reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Sbc },
            /* 0xE2 */ Invalid_Opcode!(),
            /* 0xE3 */ Invalid_Opcode!(),
            /* 0xE4 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xE5 */ Opcode {addr_m: AddrMode::ZP ,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Sbc },
            /* 0xE6 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Inc },
            /* 0xE7 */ Invalid_Opcode!(),
            /* 0xE8 */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Inc },
            /* 0xE9 */ Opcode {addr_m: AddrMode::Imm ,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Sbc },
            /* 0xEA */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Nop },
            /* 0xEB */ Invalid_Opcode!(),
            /* 0xEC */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xED */ Opcode {addr_m: AddrMode::Abs ,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Sbc },
            /* 0xEE */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Inc },
            /* 0xEF */ Invalid_Opcode!(),
            /* 0xF0 */ Opcode {addr_m: AddrMode::Rel,       reg_m: RegMod::None, flag_m: FLAG_ZERO, args: 1, ins: InsGrp::CJump },
            /* 0xF1 */ Opcode {addr_m: AddrMode::ZPIndY ,   reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Sbc },
            /* 0xF2 */ Invalid_Opcode!(),
            /* 0xF3 */ Invalid_Opcode!(),
            /* 0xF4 */ Invalid_Opcode!(),
            /* 0xF5 */ Opcode {addr_m: AddrMode::ZPX ,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Sbc },
            /* 0xF6 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Inc },
            /* 0xF7 */ Invalid_Opcode!(),
            /* 0xF8 */ Opcode { addr_m: AddrMode::Impl,       reg_m: RegMod::None, flag_m: FLAG_DEC,   args: 1, ins: InsGrp::Flag },
            /* 0xF9 */ Opcode {addr_m: AddrMode::AbsY ,     reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Sbc },
            /* 0xFA */ Invalid_Opcode!(),
            /* 0xFB */ Invalid_Opcode!(),
            /* 0xFC */ Invalid_Opcode!(),
            /* 0xFD */ Opcode {addr_m: AddrMode::AbsX ,     reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Sbc },
            /* 0xFE */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Inc },
            /* 0xFF */ Invalid_Opcode!(),
];