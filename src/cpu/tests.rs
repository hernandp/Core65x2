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

fn assert_mem_u8(sys_mem: &mem::Memory, addr: u16,  v: u8) {
    assert_eq! (sys_mem.read_byte(addr), v);
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


#[test]
fn test_sta() {
    let mut sys_mem = mem::Memory::new();
    setup_mem(&mut sys_mem);

    /* Test:
        85 80     STA $xx         ; ZP
        95 FF     STA $FF,X       ; ZPX
        8D 00 C0  STA $c000       ; Abs
        9D 78 C2  STA $C278,X     ; Abs X
        99 16 E0  STA $E016,Y     ; Abs Y (+test page-boundary cross)
        81 10     STA ($10,X)     ; Ind X
        91 2C     STA ($2C),Y     ; Ind Y (+test page-boundary cross)
        */

    sys_mem.write_vec(
        0,
        &vec![
            0x85, 0x80,
            0x95, 0xff,
            0x8d, 0x00, 0xc0,
            0x9D, 0x78, 0xC2,
            0x99, 0x16, 0xe0,
            0x81, 0x10,
            0x91, 0x2c 
        ],
    );

    // STA $80
    println!("STA $80");
    let mut sys_cpu = cpu::Cpu::new(&mut sys_mem);
    sys_cpu.regs.A = 0xAA;
    assert_eq!(sys_cpu.exec(), 3);
    assert_cpu_state(
        &sys_cpu,
        &CpuState {
            a: Some(0xaa),
            x: None,
            y: None,
            sp: None,
            pc: Some(0x0002),
            sr: Some(0),
        },
    );
    assert_mem_u8(&sys_cpu.mem, 0x0080, 0xAA);

    // STA $FF,X 
    println!("STA $FF,X");
    sys_cpu.regs.A = 0xBB;
    sys_cpu.regs.X = 0x82;
    assert_eq!(sys_cpu.exec(), 4);
    assert_cpu_state(
        &sys_cpu,
        &CpuState {
            a: Some(0xbb),
            x: Some(0x82),
            y: None,
            sp: None,
            pc: Some(0x0004),
            sr: Some(0),
        },
    );
    assert_mem_u8(&sys_cpu.mem, 0x0081, 0xBB);

    // STA $C000
    println!("STA $C000");
    sys_cpu.regs.A = 0x22;
    assert_eq!(sys_cpu.exec(), 4);
    assert_cpu_state(
        &sys_cpu,
        &CpuState {
            a: Some(0x22),
            x: None,
            y: None,
            sp: None,
            pc: Some(0x0007),
            sr: Some(0),
        },
    );
    assert_mem_u8(&sys_cpu.mem, 0xC000, 0x22);

    // Absolute,X
    // STA $C278,X
    println!("STA $C278,X");
    sys_cpu.regs.A = 0x3B;
    sys_cpu.regs.X = 0x2;
    assert_eq!(sys_cpu.exec(), 5);
    assert_cpu_state(
        &sys_cpu,
        &CpuState {
            a: Some(0x3B),
            x: Some(2),
            y: None,
            sp: None,
            pc: Some(0x000A),
            sr: Some(0),
        },
    );
    assert_mem_u8(&sys_cpu.mem, 0xc27A, 0x3b);

    // Absolute,Y
    // STA $E016,Y
    println!("STA $E016,Y");
    sys_cpu.regs.A = 0xB3;
    sys_cpu.regs.Y = 0xFF;
    assert_eq!(sys_cpu.exec(), 5); 
    assert_cpu_state(
        &sys_cpu,
        &CpuState {
            a: Some(0xb3),
            x: None,
            y: Some(0xFF),
            sp: None,
            pc: Some(0x000D),
            sr: Some(0),
        },
    );
    assert_mem_u8(&sys_cpu.mem, 0xE115, 0xB3);

    // (Indirect,X)
    // STA ($10,X) --> fetches from address ($10 + X)
    println!("STA ($10,X)");
    sys_cpu.regs.A = 0x55;
    sys_cpu.regs.X = 0x1C;
    assert_eq!(sys_cpu.exec(), 6);
    assert_cpu_state(
        &sys_cpu,
        &CpuState {
            a: Some(0x55),
            x: Some(0x1c),
            y: None,
            sp: None,
            pc: Some(0x00F),
            sr: Some(0),
        },
    );
    assert_mem_u8(&sys_cpu.mem, 0xc000, 0x55);

    // (Indirect),Y
    // STA ($2C),Y
    println!("STA ($2C),Y");
    sys_cpu.regs.A = 0x66;
    sys_cpu.regs.Y = 0x7A;
    assert_eq!(sys_cpu.exec(), 6);
    assert_cpu_state(
        &sys_cpu,
        &CpuState {
            a: Some(0x66),
            x: None,
            y: Some(0x7a),
            sp: None,
            pc: Some(0x0011),
            sr: Some(0),
        },
    );
    assert_mem_u8(&sys_cpu.mem, 0xC07A, 0x66);
}
