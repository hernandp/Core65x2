use std::io::*;
use cpu;
use cpu::opc6502;
use cpu::opc6502::opcode_table;

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
    Load(String),
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
        let cmdentry = line.chars().next();
        let cmdch = cmdentry.unwrap().to_uppercase().next(); 
        let args =  line.split_off(1);
        let argvec: Vec<&str> = args.trim().split(",").collect();

        println!("{:?}", argvec);
                
        match cmdch {
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
            Some('A') => { return Command::Asm; }
            Some('D') => { return Command::Disasm; }
            
            Some('G') => { return Command::Go;  }
            Some('L') => { /*return Command::Load("x"); */}
            Some('R') => { return Command::Reg; }
            
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
            let mut current_addr = cpu.regs.PC;
            for i in 0..10 {  
                let opcode_byte = cpu.mem.read_byte(current_addr);
                let opcode_data = &opcode_table[opcode_byte as usize];
                let instr_len = cpu.get_instr_length(&opcode_data.addr_m);
                
                print!("{:04X}    {:02X}", current_addr, opcode_byte);
                for j in 0..2 {
                    if j < instr_len - 1 {
                        print!("{:02X} ", cpu.mem.read_byte(current_addr + j as u16));
                    } 
                    else {
                        print!("   ");
                    }
                }        
                print!("{}\n",opcode_data.name);
                current_addr += instr_len as u16;
            }

        }
        Command::Quit => { return false; },
        _ => {}
    }
    true
}
