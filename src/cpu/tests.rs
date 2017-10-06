use mem;
use cpu;

use super::{FLAG_BRK, FLAG_CARRY, FLAG_DEC, FLAG_INTR, FLAG_OF, FLAG_SIGN, FLAG_ZERO};
const TEST_BYTE: u8 = 0xFE;
const ZP_TEST_BYTE: u8 = 0x4E;
const ZP_TEST_BYTE_NEG: u8 = 0xFF;

struct CpuState {
    a: Option<u8>,
    x: Option<u8>,
    y: Option<u8>,
    sr: Option<u8>,
    pc: Option<u16>,
    sp: Option<u8>,
}

fn setup_mem(sys_mem: &mut mem::Memory) {
    sys_mem.write_byte(0xC000, TEST_BYTE);
    sys_mem.write_byte(0xC07A, TEST_BYTE);
    sys_mem.write_byte(0xC27A, TEST_BYTE);
    sys_mem.write_byte(0xE115, TEST_BYTE);
    sys_mem.write_byte(0x0010, ZP_TEST_BYTE);
    sys_mem.write_byte(0x0080, ZP_TEST_BYTE);
    sys_mem.write_byte(0x0081, ZP_TEST_BYTE_NEG);
    sys_mem.write_byte(0x002C, 0x00);
    sys_mem.write_byte(0x002D, 0xC0);
}

fn assert_cpu_state(cpu: &cpu::Cpu, cpu_state: &CpuState) {
    if cpu_state.a.is_some() {
        assert_eq!(cpu.regs.A, cpu_state.a.unwrap());
    }
    if cpu_state.x.is_some() {
        assert_eq!(cpu.regs.X, cpu_state.x.unwrap());
    }
    if cpu_state.y.is_some() {
        assert_eq!(cpu.regs.Y, cpu_state.y.unwrap());
    }
    if cpu_state.sr.is_some() {
        assert_eq!(cpu.regs.SR, cpu_state.sr.unwrap());
    }
    if cpu_state.sp.is_some() {
        assert_eq!(cpu.regs.SP, cpu_state.sp.unwrap());
    }
    if cpu_state.pc.is_some() {
        assert_eq!(cpu.regs.PC, cpu_state.pc.unwrap());
    }
}

#[test]
fn test_lda() {
    let mut sys_mem = mem::Memory::new();
    setup_mem(&mut sys_mem);

    /* Test:

        A9 01     LDA #$01        ; Immediate
        A5 80     LDA $80         ; ZP
        B5 FF     LDA $FF,X       ; ZPX
        AD 00 C0  LDA $c000       ; Abs
        BD 78 C2  LDA $C278,X     ; Abs X
        B9 16 E0  LDA $E016,Y     ; Abs Y (+test page-boundary cross)
        A1 10     LDA ($10,X)     ; Ind X
        B1 2C     LDA ($2C),Y     ; Ind Y (+test page-boundary cross)
        */

    sys_mem.write_vec(
        0,
        &vec![
            0xA9, 0x01,
            0xA5, 0x80,
            0xB5, 0xff,
            0xad, 0x00, 0xc0,
            0xbd, 0x78, 0xC2,
            0xb9, 0x16, 0xe0,
            0xa1, 0x10, 
            0xb1, 0x2c,
        ],
    );

    // LDA #$01
    println!("LDA $#01");
    let mut sys_cpu = cpu::Cpu::new(&mut sys_mem);
    assert_eq!(sys_cpu.exec(), 2);
    assert_cpu_state(
        &sys_cpu,
        &CpuState {
            a: Some(0x01),
            x: None,
            y: None,
            sp: None,
            pc: Some(0x0002),
            sr: Some(0),
        },
    );

    // LDA $80
    println!("LDA $80");
    assert_eq!(sys_cpu.exec(), 3);
    assert_cpu_state(
        &sys_cpu,
        &CpuState {
            a: Some(ZP_TEST_BYTE),
            x: None,
            y: None,
            sp: None,
            pc: Some(0x0004),
            sr: Some(0),
        },
    );

    // LDA $FF,X --- should fetch value from $FF + X -> $FF + $82 = $81
    println!("LDA $FF,X");
    sys_cpu.regs.SR = 0;
    sys_cpu.regs.X = 0x82;
    assert_eq!(sys_cpu.exec(), 4);
    assert_cpu_state(
        &sys_cpu,
        &CpuState {
            a: Some(ZP_TEST_BYTE_NEG),
            x: Some(0x82),
            y: None,
            sp: None,
            pc: Some(0x0006),
            sr: Some(FLAG_SIGN),
        },
    );

    // LDA $C000
    println!("LDA $C000");
    sys_cpu.regs.SR = 0;
    assert_eq!(sys_cpu.exec(), 4);
    assert_cpu_state(
        &sys_cpu,
        &CpuState {
            a: Some(TEST_BYTE),
            x: None,
            y: None,
            sp: None,
            pc: Some(0x0009),
            sr: Some(FLAG_SIGN),
        },
    );

    // Absolute,X
    // LDA $C278,X
    println!("LDA $C278,X");
    sys_cpu.regs.SR = 0;
    sys_cpu.regs.X = 0x2;
    assert_eq!(sys_cpu.exec(), 4);
    assert_cpu_state(
        &sys_cpu,
        &CpuState {
            a: Some(TEST_BYTE),
            x: Some(2),
            y: None,
            sp: None,
            pc: Some(0x000C),
            sr: Some(FLAG_SIGN),
        },
    );

    // Absolute,Y
    // LDA $E016,Y
    println!("LDA $E016,Y");
    sys_cpu.regs.SR = 0;
    sys_cpu.regs.Y = 0xFF;
    assert_eq!(sys_cpu.exec(), 5); // page-boundary cross
    assert_cpu_state(
        &sys_cpu,
        &CpuState {
            a: Some(TEST_BYTE),
            x: None,
            y: Some(0xFF),
            sp: None,
            pc: Some(0x000F),
            sr: Some(FLAG_SIGN),
        },
    );

    // (Indirect,X)
    // LDA ($10,X) --> fetches from address ($10 + X)
    println!("LDA ($10,X)");
    sys_cpu.regs.SR = 0;
    sys_cpu.regs.X = 0x1C;
    assert_eq!(sys_cpu.exec(), 6);
    assert_cpu_state(
        &sys_cpu,
        &CpuState {
            a: Some(TEST_BYTE),
            x: Some(0x1c),
            y: None,
            sp: None,
            pc: Some(0x0011),
            sr: Some(FLAG_SIGN),
        },
    );

    // (Indirect),Y
    // LDA ($2C),Y
    println!("LDA ($2C),Y");
    sys_cpu.regs.SR = 0;
    sys_cpu.regs.Y = 0x7A;
    assert_eq!(sys_cpu.exec(), 6);
    assert_cpu_state(
        &sys_cpu,
        &CpuState {
            a: Some(TEST_BYTE),
            x: None,
            y: Some(0x7a),
            sp: None,
            pc: Some(0x0013),
            sr: Some(FLAG_SIGN),
        },
    );
}
