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

pub enum Command {
    Quit,
    Reg,
}

pub fn do_prompt() -> Command {
    let mut line = String::new();
    print!(".");
    stdout().flush();
    stdin().read_line(&mut line);

    line = String::from(line.trim());

    // 1) Separate command from the rest of input, if any
    let tokens: Vec<&str> = line.split_whitespace().collect();
    if tokens.len() != 0 {
        let cmdch = String::from(tokens[0]).chars().next();
        match cmdch.unwrap() {
            'Q' => println!("QUIT"),
            'A' => println!("ASS"),

            '?' => println!("Help"),
            _ => println!("?"),
        }
    }
    Command::Quit
}

pub fn exec(cmd: &Command, cpu: &mut cpu::Cpu) -> bool {
    true
}
