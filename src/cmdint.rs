use std::io::*;
use std::fs::*;
use cpu;
use cpu::opc6502::OPCODE_TABLE;

#[derive(PartialEq)]
pub enum Command {
    Asm,
    Disasm { start_addr: Option<u16>, length: Option<u16> },
    Enter,
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
        let argvec: Vec<&str> = if args.len() == 0 { Vec::new() } else { args.trim().split(",").collect() };
                
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
            Some('A') => return Command::Asm,
            Some('D') => {
                if argvec.len() > 0 {
                    if argvec[0] == "?" {
                        println!("D[start_address][,len]\tDisassemble\n");
                        println!("start_address\t\tThe memory address where disassembly starts. ");
                        println!("             \t\tIf unspecified, current program counter is assumed.");
                        println!("len          \t\tLength in bytes to dissasemble. Default is 32.");
                        return Command::Null
                    }
                }
                let start: Option<u16> = if argvec.len() > 0 { Some(argvec[0].parse::<u16>().unwrap()) } else { None };
                let len: Option<u16> = if argvec.len() == 2 { Some(argvec[1].parse::<u16>().unwrap()) } else { None};
           
                return Command::Disasm { start_addr: start, length: len };
            },
            Some('E') => return Command::Enter,
            Some('G') => return Command::Go,
            Some('L') => { 
                 if argvec.len() > 0 {
                    if argvec[0] == "?" {
                        println!("L[filename][,addr]\tLoad program in memory\n");
                        println!("filename\t\tBinary image with code to load. Should not exceed 64K.");
                        println!("addr    \t\tAddress where the code is going to be located.");
                        return Command::Null
                    }
                }
                if argvec.len() == 0 {
                    println!("?Filename not specified  error");
                    return Command::Null;
                }               
              
                return Command::Load(String::from(argvec[0]));
            },
            Some('M') => return Command::Mem,
            Some('R') => return Command::Reg,
            
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
        Command::Disasm { start_addr, length } => {
            let mut current_addr = cpu.regs.PC;
            for _ in 0..10 {  
                let opcode_byte = cpu.mem.read_byte(current_addr);
                let opcode_data = &OPCODE_TABLE[opcode_byte as usize];
                let instr_len = cpu.get_instr_length(&opcode_data.addr_m);
                
                print!("{:04X}    {:02X} ", current_addr, opcode_byte);
                for j in 1..3 {
                    if j < instr_len  {
                        print!("{:02X} ", cpu.mem.read_byte(current_addr + j as u16));
                    } 
                    else {
                        print!("   ");
                    }
                }        

                let op0 = cpu.mem.read_byte(current_addr + 1 as u16);
                let op1 = cpu.mem.read_byte(current_addr + 2 as u16);

                print!("    {} {}\n",opcode_data.name,
                match (*opcode_data).addr_m {
                    cpu::AddrMode::Acc =>  format!("A"),
                    cpu::AddrMode::Imm  => format!("#${:02X}", op0),
                    cpu::AddrMode::ZP   => format!("${:02X}", op0),
                    cpu::AddrMode::ZPX  => format!("${:02X},X", op0),
                    cpu::AddrMode::ZPY  => format!("${:02X},Y", op0),
                    cpu::AddrMode::Abs  => format!("${:02X}{:02X}", op1, op0),
                    cpu::AddrMode::AbsX  => format!("${:02X}{:02X},X", op1, op0),
                    cpu::AddrMode::AbsY  => format!("${:02X}{:02X},Y", op1, op0),
                    cpu::AddrMode::ZPIndX=> format!("(${:02X},X)", op0),
                    cpu::AddrMode::ZPIndY=> format!("(${:02X}),Y", op0),
                    cpu::AddrMode::Rel   => format!("${:02X}", op0),
                    cpu::AddrMode::Ind   => format!("(${:04X}{:04X})", op1, op0),
                    cpu::AddrMode::Impl   => format!("")                    
                });
                current_addr += instr_len as u16;
            }

        },
        Command::Mem => {
            let mut current_addr = cpu.regs.PC;
            for _ in 0..8 {
                print!("{:04X}  ", current_addr);
                for i in 0..8 {                    
                    print!("{:02X} ", cpu.mem.read_byte(current_addr + i));
                }
                for i in 0..8 {                    
                    print!(".");
                    //print!("{}", cpu.mem.read_byte(current_addr + i));
                }
                println!("");
                current_addr += 8;
            }
        },
        Command::Load(ref filename) => {
            let binary_file = File::open(filename);
            if binary_file.is_err()
            {
                println!("?Error opening file: {}", binary_file.err().unwrap())
            }
            else 
            {
                let mut buf: Vec<u8> = Vec::new();
                let res = binary_file.unwrap().read_to_end(&mut buf);
                if res.is_err()
                {
                    println!("?I/O Error: {}", res.err().unwrap());
                }
                else
                {
                    cpu.mem.write_vec(0, &buf);
                    println!("Read {} bytes to address {:04X}", res.unwrap(), 0);
                }
            }
        },
        Command::Enter => {
            let mut current_addr = cpu.regs.PC;
            loop {
                let mut entry = String::new();
                print!("{:04X}: ", current_addr);
                stdout().flush().unwrap();
                stdin().read_line(&mut entry).expect("read_line from stdin failed");

                entry = String::from(entry.trim());
                if entry.len() > 0 {

                    // Validate all entries first.
                    let mut bytes: Vec<u8> = Vec::new();

                    for e in entry.split(",").collect::<Vec<&str>>() {
                        let res = u8::from_str_radix(e, 16);
                        if res.is_err() {
                            println!("?Invalid byte entry: {}", e);
                        } 
                        else {
                            bytes.push(res.unwrap());
                        }
                    }

                    cpu.mem.write_vec(current_addr, &bytes);
                    current_addr += bytes.len() as u16;
                }
                else {
                    break;
                }
            }
        },
        Command::Quit => { return false; },
        _ => {}
    }
    true
}
