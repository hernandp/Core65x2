use cpu::AddrMode;
use cpu::RegMod;
use cpu::{ FLAG_CARRY, FLAG_DEC, FLAG_INTR, FLAG_OF, FLAG_SIGN, FLAG_ZERO};

enum InsGrp {
    Load, Store, Tx_from_A, Tx_from_X, Tx_from_Y, Tx_from_SP,
    Push, Pull,  Or, And, Xor, Adc, Sbc, Cmp, Dec, Inc, Shift, Rot,
    Jump,  Jsr, Rti, Rts, CJump, Bit, Brk, Flag, Nop
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

const opcode_table: &'static [Opcode] = &[  
            // LDA
            /* 0xA9 */ Opcode {addr_m: AddrMode::Imm,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xA5 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xB5 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xA1 */ Opcode {addr_m: AddrMode::ZPIndX,    reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xB1 */ Opcode {addr_m: AddrMode::ZPIndY,    reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xAD */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xBD */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xB9 */ Opcode {addr_m: AddrMode::AbsY,      reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Load},

            // LDX
            /* 0xA2 */ Opcode {addr_m: AddrMode::Imm,       reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xA6 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xB6 */ Opcode {addr_m: AddrMode::ZPY,       reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xAE */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xBE */ Opcode {addr_m: AddrMode::AbsY,      reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Load},

            // LDY
            /* 0xA0 */ Opcode {addr_m: AddrMode::Imm,       reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xA4 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xB4 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xAC */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Load},
            /* 0xBC */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Load},

            // STA
            /* 0x85 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x95 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x81 */ Opcode {addr_m: AddrMode::ZPIndX,    reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x91 */ Opcode {addr_m: AddrMode::ZPIndY,    reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x8D */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x9D */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x99 */ Opcode {addr_m: AddrMode::AbsY,      reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Store},

            // STX
            /* 0x86 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x96 */ Opcode {addr_m: AddrMode::ZPY,       reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x8E */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Store},

            // STY
            /* 0x84 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x94 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Store},
            /* 0x8C */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Store},

            // TAX, TXA, TAY, TYA, TSX, TXS
            /* 0xAA */ Opcode { addr_m: AddrMode::Impl,     reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Tx_from_A },
            /* 0x8A */ Opcode { addr_m: AddrMode::Impl,     reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Tx_from_X },
            /* 0xA8 */ Opcode { addr_m: AddrMode::Impl,     reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Tx_from_A },
            /* 0x98 */ Opcode { addr_m: AddrMode::Impl,     reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Tx_from_Y },
            /* 0xBA */ Opcode { addr_m: AddrMode::Impl,     reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Tx_from_SP},
            /* 0x9A */ Opcode { addr_m: AddrMode::Impl,     reg_m: RegMod::SP,flag_m: 0, args: 0, ins: InsGrp::Tx_from_X },

            // PLA / PHA / PLP / PHP (stack)
            /* 0x68 */ Opcode { addr_m: AddrMode::Impl,     reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Pull },
            /* 0x48 */ Opcode { addr_m: AddrMode::Impl,     reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Push },
            /* 0x28 */ Opcode { addr_m: AddrMode::Impl,     reg_m: RegMod::SR,flag_m: 0, args: 0, ins: InsGrp::Pull },
            /* 0x08 */ Opcode { addr_m: AddrMode::Impl,     reg_m: RegMod::SR,flag_m: 0, args: 0, ins: InsGrp::Push },

            /* Logical / Arithmetic */

            // ORA
            /* 0x09 */ Opcode {addr_m: AddrMode::Imm,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Or },
            /* 0x05 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Or },
            /* 0x15 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Or },
            /* 0x01 */ Opcode {addr_m: AddrMode::ZPIndX,    reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Or },
            /* 0x11 */ Opcode {addr_m: AddrMode::ZPIndY,    reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Or },
            /* 0x0D */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Or },
            /* 0x1D */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Or },
            /* 0x19 */ Opcode {addr_m: AddrMode::AbsY,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Or },

            // AND
            /* 0x29 */ Opcode {addr_m: AddrMode::Imm,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::And },
            /* 0x25 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::And },
            /* 0x35 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::And },
            /* 0x21 */ Opcode {addr_m: AddrMode::ZPIndX,    reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::And },
            /* 0x31 */ Opcode {addr_m: AddrMode::ZPIndY,    reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::And },
            /* 0x2D */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::And },
            /* 0x3D */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::And },
            /* 0x39 */ Opcode {addr_m: AddrMode::AbsY,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::And },

            // EOR
            /* 0x49 */ Opcode {addr_m: AddrMode::Imm,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Xor },
            /* 0x45 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Xor },
            /* 0x55 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Xor },
            /* 0x41 */ Opcode {addr_m: AddrMode::ZPIndX,    reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Xor },
            /* 0x51 */ Opcode {addr_m: AddrMode::ZPIndY,    reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Xor },
            /* 0x4D */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Xor },
            /* 0x5D */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Xor },
            /* 0x59 */ Opcode {addr_m: AddrMode::AbsY,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Xor },

            // ADC
            /* 0x69 */ Opcode {addr_m: AddrMode::Imm,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Adc },
            /* 0x65 */ Opcode {addr_m: AddrMode::ZP ,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Adc },
            /* 0x75 */ Opcode {addr_m: AddrMode::ZPX ,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Adc },
            /* 0x61 */ Opcode {addr_m: AddrMode::ZPIndX ,   reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Adc },
            /* 0x71 */ Opcode {addr_m: AddrMode::ZPIndY ,   reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Adc },
            /* 0x6D */ Opcode {addr_m: AddrMode::Abs ,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Adc },
            /* 0x7D */ Opcode {addr_m: AddrMode::AbsX ,     reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Adc },
            /* 0x79 */ Opcode {addr_m: AddrMode::AbsY ,     reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Adc },

            // SBC
            /* 0xE9 */ Opcode {addr_m: AddrMode::Imm ,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Sbc },
            /* 0xE5 */ Opcode {addr_m: AddrMode::ZP ,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Sbc },
            /* 0xF5 */ Opcode {addr_m: AddrMode::ZPX ,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Sbc },
            /* 0xE1 */ Opcode {addr_m: AddrMode::ZPIndX ,   reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Sbc },
            /* 0xF1 */ Opcode {addr_m: AddrMode::ZPIndY ,   reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Sbc },
            /* 0xED */ Opcode {addr_m: AddrMode::Abs ,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Sbc },
            /* 0xFD */ Opcode {addr_m: AddrMode::AbsX ,     reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Sbc },
            /* 0xF9 */ Opcode {addr_m: AddrMode::AbsY ,     reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Sbc },

            // CMP
            /* 0xC9 */ Opcode {addr_m: AddrMode::Imm,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xC5 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xD5 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xC1 */ Opcode {addr_m: AddrMode::ZPIndX,    reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xD1 */ Opcode {addr_m: AddrMode::ZPIndY,    reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xCD */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xDD */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xD9 */ Opcode {addr_m: AddrMode::AbsY,      reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Cmp },

            // CPX / CPY
            /* 0xE0 */ Opcode {addr_m: AddrMode::Imm,       reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xE4 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xEC */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xC0 */ Opcode {addr_m: AddrMode::Imm,       reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xC4 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Cmp },
            /* 0xCC */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Cmp },

            // DEC
            /* 0xC6 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Dec },
            /* 0xD6 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Dec },
            /* 0xCE */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Dec },
            /* 0xDE */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Dec },

            // DEX/DEY
            /* 0xCA */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Dec },
            /* 0x88 */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Dec },

            // INC
            /* 0xE6 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Inc },
            /* 0xF6 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Inc },
            /* 0xEE */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Inc },
            /* 0xFE */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::A, flag_m: 0, args: 0, ins: InsGrp::Inc },

            // INX/INY
            /* 0xE8 */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::X, flag_m: 0, args: 0, ins: InsGrp::Inc },
            /* 0xC8 */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::Y, flag_m: 0, args: 0, ins: InsGrp::Inc },

            // ASL
            /* 0x0A */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Shift },
            /* 0x06 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Shift },
            /* 0x16 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Shift },
            /* 0x0E */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Shift },
            /* 0x1E */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Shift },

            // ROL
            /* 0x2A */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Rot },
            /* 0x26 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Rot },
            /* 0x36 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Rot },
            /* 0x2E */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Rot },
            /* 0x3E */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Rot },

            // LSR
            /* 0x4A */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::None, flag_m: 0, args: 1, ins: InsGrp::Shift },
            /* 0x46 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::None, flag_m: 0, args: 1, ins: InsGrp::Shift },
            /* 0x56 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::None, flag_m: 0, args: 1, ins: InsGrp::Shift },
            /* 0x4E */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::None, flag_m: 0, args: 1, ins: InsGrp::Shift },
            /* 0x5E */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::None, flag_m: 0, args: 1, ins: InsGrp::Shift },

            // ROR
            /* 0x6A */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::None, flag_m: 0, args: 1, ins: InsGrp::Rot },
            /* 0x66 */ Opcode {addr_m: AddrMode::ZP,        reg_m: RegMod::None, flag_m: 0, args: 1, ins: InsGrp::Rot },
            /* 0x76 */ Opcode {addr_m: AddrMode::ZPX,       reg_m: RegMod::None, flag_m: 0, args: 1, ins: InsGrp::Rot },
            /* 0x6E */ Opcode {addr_m: AddrMode::Abs,       reg_m: RegMod::None, flag_m: 0, args: 1, ins: InsGrp::Rot },
            /* 0x7E */ Opcode {addr_m: AddrMode::AbsX,      reg_m: RegMod::None, flag_m: 0, args: 1, ins: InsGrp::Rot },

            /* Jump and program-counter change instructions */

            // BPL / BMI / BVC / BVS / BCC / BCS / BNE / BEQ
            /* 0x10 */ Opcode {addr_m: AddrMode::Rel,       reg_m: RegMod::None, flag_m: FLAG_SIGN, args: 0, ins: InsGrp::CJump },
            /* 0x30 */ Opcode {addr_m: AddrMode::Rel,       reg_m: RegMod::None, flag_m: FLAG_SIGN, args: 1, ins: InsGrp::CJump },
            /* 0x50 */ Opcode {addr_m: AddrMode::Rel,       reg_m: RegMod::None, flag_m: FLAG_OF,   args: 0, ins: InsGrp::CJump },
            /* 0x70 */ Opcode {addr_m: AddrMode::Rel,       reg_m: RegMod::None, flag_m: FLAG_OF,   args: 1, ins: InsGrp::CJump },
            /* 0x90 */ Opcode {addr_m: AddrMode::Rel,       reg_m: RegMod::None, flag_m: FLAG_CARRY,args: 0, ins: InsGrp::CJump },
            /* 0xB0 */ Opcode {addr_m: AddrMode::Rel,       reg_m: RegMod::None, flag_m: FLAG_CARRY,args: 1, ins: InsGrp::CJump },
            /* 0xD0 */ Opcode {addr_m: AddrMode::Rel,       reg_m: RegMod::None, flag_m: FLAG_ZERO, args: 0, ins: InsGrp::CJump },
            /* 0xF0 */ Opcode {addr_m: AddrMode::Rel,       reg_m: RegMod::None, flag_m: FLAG_ZERO, args: 1, ins: InsGrp::CJump },

            // BRK/RTI
            /* 0x00 */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Brk },
            /* 0x40 */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Rti },

            // JSR/RTS
            /* 0x20 */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Jsr },
            /* 0x60 */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Rts },

            // JMP
            /* 0x4C */ Opcode { addr_m: AddrMode::Abs ,     reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Jump },
            /* 0x6C */ Opcode { addr_m: AddrMode::Ind ,     reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Jump },

            // BIT
            /* 0x24 */ Opcode { addr_m: AddrMode::ZP ,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Bit },
            /* 0x2C */ Opcode { addr_m: AddrMode::Abs,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Bit },

            /* Flag instructions */

            // SEC/CLD/SED/CLI/SEI/CLV
            /* 0x18 */ Opcode { addr_m: AddrMode::Impl,       reg_m: RegMod::None, flag_m: FLAG_CARRY, args: 0, ins: InsGrp::Flag },
            /* 0x38 */ Opcode { addr_m: AddrMode::Impl,       reg_m: RegMod::None, flag_m: FLAG_CARRY, args: 1, ins: InsGrp::Flag },
            /* 0xD8 */ Opcode { addr_m: AddrMode::Impl,       reg_m: RegMod::None, flag_m: FLAG_DEC,   args: 0, ins: InsGrp::Flag },
            /* 0xF8 */ Opcode { addr_m: AddrMode::Impl,       reg_m: RegMod::None, flag_m: FLAG_DEC,   args: 1, ins: InsGrp::Flag },
            /* 0x58 */ Opcode { addr_m: AddrMode::Impl,       reg_m: RegMod::None, flag_m: FLAG_INTR,  args: 0, ins: InsGrp::Flag },
            /* 0x78 */ Opcode { addr_m: AddrMode::Impl,       reg_m: RegMod::None, flag_m: FLAG_INTR,  args: 1, ins: InsGrp::Flag },
            /* 0xB8 */ Opcode { addr_m: AddrMode::Impl,       reg_m: RegMod::None, flag_m: FLAG_OF,    args: 0, ins: InsGrp::Flag },

            /* No-op */
            /* 0xEA */ Opcode {addr_m: AddrMode::Impl,      reg_m: RegMod::None, flag_m: 0, args: 0, ins: InsGrp::Nop },
];