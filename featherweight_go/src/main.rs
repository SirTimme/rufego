extern crate peg;
extern crate common;

mod parser;
mod type_checker;
mod interpreter;

use std::collections::{HashMap};
use std::fs::read_to_string;
use common::token::{lex_program};
use interpreter::{evaluate_expression};
use parser::language::{parse_program};
use type_checker::{create_type_infos, program_well_formed};

fn main() {
    let source_file = match read_to_string("featherweight_go/input/input.go") {
        Ok(source) => source,
        Err(error) => {
            eprintln!("Reading the source file failed: {error}");
            return;
        }
    };

    let tokens = match lex_program(source_file.as_str()) {
        Ok(tokens) => tokens,
        Err(error) => {
            eprintln!("{}", error.message);
            return;
        }
    };
    
    let program = match parse_program(&tokens) {
        Ok(program) => program,
        Err(error) => {
            eprintln!("Parsing the program failed: {error}");
            return;
        }
    };

    let type_infos = match create_type_infos(&program) {
        Ok(type_infos) => type_infos,
        Err(error) => {
            eprintln!("Creating the type infos failed: {}", error.message);
            return;
        }
    };

    match program_well_formed(&program, &type_infos) {
        Ok(_) => {}
        Err(error) => {
            eprintln!("Program is not well-formed:\n{}", error.message);
            return;
        },
    };

    let value = match evaluate_expression(&program.expression, &HashMap::new(), &type_infos) {
        Ok(value) => value,
        Err(error) => {
            eprintln!("Evaluating the program failed: {}", error.message);
            return;
        },
    };

    println!("{:#?}", value);
}
