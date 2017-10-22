use std::io::*;
use std::fs::*;
use cpu;
use cpu::opc6502::OPCODE_TABLE;

#[derive(PartialEq)]
pub enum Command {
    Asm,
    Disasm { start_addr: u16, length: u16 },
    Enter,
    Mem,
    Go,
    Load { filename: String, start_addr: u16 },
    Null,
    Quit,
    ResetCPU,
    Reg { reg: String, val: u16 },    
    Step
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

        if argvec.len() > 0 && argvec[0] == "?" {
            display_cmd_help(cmdch.unwrap());
            return Command::Null;
        }
                
        match cmdch {
            Some('!') => {
                  if argvec.len() == 0 {
                    println!("?Extended command missing  error");
                    return Command::Null;
                  }
                  
                  match argvec[0].to_uppercase().as_ref() {
                      "RESET" => { 
                          let ch = ask_choice("Reset CPU (Y/N)?", &vec!['y','n']);
                          if ch == 'Y' {
                              return Command::ResetCPU;
                          }
                       }
                      _ => { println!("?Unknown extended command: {}", argvec[1])}
                  }
            }
            Some('Q') => {
                let ch = ask_choice("Do you want to quit (Y/N)?", &vec!['y','n']);
                if ch == 'Y' {
                    return Command::Quit;
                }
                    return Command::Null;
            }
            Some('A') => return Command::Asm,
            Some('D') => {

                let mut startaddr:u16 = 0;
                let mut len:u16 = 32;

                if argvec.len() == 1 {
                    let v = u16::from_str_radix(argvec[0], 16);
                    if v.is_err() {
                        println!("?Invalid address: {}", argvec[0]);
                        return Command::Null;
                    }
                    else {
                        startaddr = v.unwrap();
                    }
                }

                if argvec.len() == 2 {
                    let v = u16::from_str_radix(argvec[1], 16);
                    if v.is_err() {
                        println!("?Invalid length: {}", argvec[1]);
                        return Command::Null;
                    }
                    else {
                        len = v.unwrap();
                    }   
                }
           
                return Command::Disasm { start_addr: startaddr, length: len };
            },
            Some('E') => return Command::Enter,
            Some('G') => return Command::Go,
            Some('L') => { 
                  if argvec.len() == 0 {
                    println!("?Filename not specified  error");
                    return Command::Null;
                }     

                let mut startaddr:u16 = 0;
                if argvec.len() > 1 {
                    let ad = u16::from_str_radix(argvec[1], 16);
                    if ad.is_err() {
                        println!("?Invalid address: {}", argvec[1]);
                        return Command::Null;
                    }
                    else {
                        startaddr = ad.unwrap();
                    }
                }                         
                return Command::Load{ filename: String::from(argvec[0]), start_addr: startaddr };
            },
            Some('M') => return Command::Mem,
            Some('R') => {

                if argvec.len() == 1 {
                    println!("?Missing register value  error");
                    return Command::Null;
                }

                let mut regname: String = String::new();
                let mut regval:  u16 = 0xffff;

                if argvec.len() > 1 {
                    regname = argvec[0].to_uppercase();
                    if regname != "X" && regname != "SR" && regname != "Y" && regname != "A" && regname != "PC" && regname != "SP" {
                        println!("?Invalid register. Use A,X,Y,PC,SP,SR");
                        return Command::Null;
                    }
                                        
                    let v = u16::from_str_radix(argvec[1], 16);
                    if v.is_err() {
                        println!("?Invalid value: {}", argvec[1]);
                        return Command::Null;
                    }
                    else {
                        regval = v.unwrap();
                    }
                }    

                return Command::Reg { reg: regname, val: regval };
            }
            Some ('S') => return Command::Step,            
            Some ('?') => display_help(),
            _ => println!("?Unknown command {}", cmdch.unwrap()),
        }
    }

    Command::Null
}

pub fn exec(cmd: &Command, cpu: &mut cpu::Cpu) -> bool {
    match *cmd {
        Command::Go => {
            cpu.exec();
        },
        Command::ResetCPU => {
            cpu.reset();
            exec(&Command::Reg { reg: String::new(), val:0 }, cpu);
        }
        Command::Reg { ref reg, val }=> {
            if reg == "" {
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
            } 
            else {
                match reg.as_ref() {
                    "X" => cpu.regs.X = val as u8,
                    "Y" => cpu.regs.Y = val as u8,
                    "A" => cpu.regs.A = val as u8,
                    "SP" => cpu.regs.SP = val as u8,
                    "SR" => cpu.regs.SR = val as u8,
                    "PC" => cpu.regs.PC = val,                    
                    _ => panic!()
                }   
                if reg == "PC" { 
                    println!("{} = {:04X}", reg, val);
                } 
                else { 
                    println!("{} = {:02X}", reg, val);
                }                          
            }
        },
        Command::Disasm { start_addr, length } => {
            let mut current_addr = start_addr;
            let mut byte_count: u32 = 0;
            loop {  
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
                byte_count += instr_len;

                if byte_count >= length as u32 {
                    break;
                }
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
                    let cb = cpu.mem.read_byte(current_addr + i);
                    if cb >= 32 {
                        print!("{}", cpu.mem.read_byte(current_addr + i) as char);
                    }
                    else {
                        print!(".");
                    }
                }
                println!("");
                current_addr += 8;
            }
        },
        Command::Load{ ref filename, start_addr } => {
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
                    cpu.mem.write_vec(start_addr, &buf);
                    println!("Read {} bytes to address {:04X}", res.unwrap(), start_addr);
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

fn ask_choice(text: &str, options: &Vec<char>) -> char {
    print!("{}",text);
    stdout().flush().unwrap();
    loop {
        
        let mut r = String::new();
        stdin()
            .read_line(&mut r)
            .expect("read_line from stdin failed");

        r = String::from(r.trim()).to_uppercase();
        let ch = r.chars().next();
        if ch.is_some() {
            let b = options.into_iter().find( |&c| c.to_uppercase().nth(0).unwrap() == ch.unwrap());

            if b.is_some() {
                return ch.unwrap();
            }
        }
    }
}

fn display_cmd_help(c: char) {
    match c {
        'D' => { println!("D[start_address][,len]\tDisassemble\n");
                 println!("start_address\t\tThe memory address where disassembly starts. ");
                 println!("             \t\tIf unspecified, current program counter is assumed.");
                 println!("len          \t\tLength in bytes to dissasemble. Default is 32.");
        }
        'L' => { println!("L[filename][,addr]\tLoad program in memory\n");
                 println!("filename\t\tBinary image with code to load. Should not exceed 64K.");
                 println!("addr    \t\tAddress where the code is going to be located.");
        }
        'R' => { println!("R[reg,value]\tDisplay or modify registers\n");
                 println!("reg,value\t\tRegistry with value to modify. If unspecified, displays");
                 println!("         \t\tcurrent processor registers."); 
        }
        _ => { println!("No help for this command."); }
        
    }
}

fn display_help() {
    println!("Commands: ");
    println!("A         Assemble");
    println!("D         Dissassemble");
    println!("E         Enter values in memory");
    println!("G         Go (execute CPU)");
    println!("L         Load file");
    println!("R         Display or modify registers");
    println!("S         Step");
    println!("----------------------------------------------------------------");
    println!("Extended commands:");
    println!("!LICENSE  Display program license text");
    println!("!RESET    Reset CPU");
    println!("!MODEL    Set CPU model (6502, 65C02 or 65CE02)");
    println!("!CLOCK    Set CPU clock in MHz");
}