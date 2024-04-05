use parser::parse_and_simplify;
use std::{env, process::exit};

mod interp;
mod parser;

fn main() {
    if env::args().len() != 2 {
        eprintln!("Usage: {} <file.lua>", env::args().next().unwrap());
        exit(1)
    }
    let filename = env::args().skip(1).next().unwrap();
    let str = std::fs::read_to_string(filename).unwrap();
    let ast = parse_and_simplify(&str).unwrap_or_else(|s| {
        eprintln!("Error: {}", s);
        exit(1)
    });
    interp::run(&ast);
}
