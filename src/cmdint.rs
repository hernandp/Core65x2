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
            Some('A') => println!("ASS"),
            Some('?') => println!("Help"),
            _ => println!("?"),
        }
    }

    Command::Null
}


pub fn exec(cmd: &Command, cpu: &mut cpu::Cpu) -> bool {
    true
}
