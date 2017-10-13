use std::io::*;
use cpu;

enum RegMod {
    PC,
    XR,
    YR,
    AC,
    SP,
    SR,
}

#[derive(PartialEq)]
pub enum Command {
    Asm,
    Disasm,
    Mem,
    Go,
    Null,
    Quit,
    Reg,    
}


pub fn do_prompt() -> Command {
    let mut line = String::new();
    print!(".");
    stdout().flush().unwrap();
    stdin()
        .read_line(&mut line)
        .expect("read_line from stdin failed");

    line = String::from(line.trim());
    if line.len() > 0 {
        let cmdch = line.chars().next();
        match cmdch.unwrap().to_uppercase().next() {
            Some('Q') => {
                print!("Do you want to quit (Y/N)?");
                stdout().flush().unwrap();

                loop {
                    let mut r = String::new();
                    stdin()
                        .read_line(&mut r)
                        .expect("read_line from stdin failed");

                    r = String::from(r.trim()).to_uppercase();
                    let ch = r.chars().next();

                    if ch.is_some() && (ch.unwrap() == 'Y' || ch.unwrap() == 'y') {
                        return Command::Quit;
                    }
                    if ch.is_some() && (ch.unwrap() == 'N' || ch.unwrap() == 'n') {
                        return Command::Null;
                    }
                }
            }
            Some('R') => { return Command::Reg; }
            Some('A') => { return Command::Asm; }
            Some('G') => { return Command::Go;  }
            Some('D') => { return Command::Disasm; }
            Some('?') => println!("Help"),
            _ => println!("?"),
        }
    }

    Command::Null
}

pub fn exec(cmd: &Command, cpu: &mut cpu::Cpu) -> bool {
    match *cmd {
        Command::Go => {
            let clk = cpu.exec();
        },
        Command::Reg => {
            println!("PC      N V - B D I Z C    AC  XR  YR  SP");
            println!("{:04X}    {rN} {rV}   {rB} {rD} {rI} {rZ} {rC}    {ac:02X}  {xr:02X}  {yr:02X}  {sp:02X}", regs = cpu.regs.PC, 
            ac = cpu.regs.A, xr = cpu.regs.X, yr = cpu.regs.Y, sp = cpu.regs.SP,
            rB = if cpu.is_flag_on(cpu::FLAG_BRK) { '1' } else { '.' },
            rN = if cpu.is_flag_on(cpu::FLAG_SIGN) { '1' } else { '.' },
            rV = if cpu.is_flag_on(cpu::FLAG_OF) { '1' } else { '.' },
            rD = if cpu.is_flag_on(cpu::FLAG_DEC) { '1' } else { '.' },
            rI = if cpu.is_flag_on(cpu::FLAG_INTR) { '1' } else { '.' },
            rZ = if cpu.is_flag_on(cpu::FLAG_ZERO) { '1' } else { '.' },
            rC = if cpu.is_flag_on(cpu::FLAG_CARRY) { '1' } else { '.' });
        },
        Command::Disasm => {

        }
        Command::Quit => { return false; },
        _ => {}
    }
    true
}
