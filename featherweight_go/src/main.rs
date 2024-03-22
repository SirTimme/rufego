extern crate logos;
extern crate peg;

mod token;
mod parser;
mod type_checker;
mod interpreter;

use std::collections::{HashMap};
use std::fs::read_to_string;
use logos::Logos;
use interpreter::{evaluate_expression};
use parser::language::{parse_program};
use token::{Token};
use type_checker::{build_type_infos, program_well_formed};

fn main() {
    let source_file = match read_to_string("featherweight_go/input/input.go") {
        Ok(source) => source,
        Err(error) => {
            eprintln!("Reading the source file failed: {error}");
            return;
        }
    };

    let tokens = match Token::lexer(&source_file).collect::<Result<Vec<Token>, ()>>() {
        Ok(tokens) => tokens,
        Err(_) => {
            eprintln!("Lexing the source failed");
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

    let type_infos = match build_type_infos(&program) {
        Ok(type_infos) => type_infos,
        Err(error) => {
            eprintln!("Creating the type infos failed: {}", error.message);
            return;
        }
    };

    match program_well_formed(&program, &type_infos) {
        Ok(_) => {
            match evaluate_expression(&program.expression, &HashMap::new(), &type_infos) {
                Ok(value) => println!("{:#?}", value),
                Err(error) => eprintln!("Evaluating the program failed: {}", error.message),
            }
        }
        Err(error) => eprintln!("Program is not well-formed:\n{}", error.message),
    }
}
