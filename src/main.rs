extern crate logos;
extern crate peg;

mod token;
mod parser;
mod type_checker;
mod interpreter;

use std::collections::{HashMap};
use std::fs::read_to_string;
use logos::Logos;
use peg::error::ParseError;
use interpreter::{evaluate};
use parser::{Program};
use parser::language::program;
use token::Token;
use type_checker::{build_type_infos, check_program};

fn main() {
    if let Ok(source) = read_to_string("input/input.fgo") {
        let tokens: Result<Vec<Token>, ()> = Token::lexer(&source).collect();
        let tokens = tokens.map_err(|_| println!("ERROR: Lexer encountered an unknown token")).unwrap();

        let result: Result<Program, ParseError<usize>> = program(&tokens);
        let result = result.map_err(|error| println!("ERROR: {error}")).unwrap();

        let type_infos = build_type_infos(&result);

        check_program(&result, &type_infos);

        eprintln!("Result of program is: {}", evaluate(&result.expression, &HashMap::new(), &type_infos));
    }
}