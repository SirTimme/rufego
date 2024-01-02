extern crate logos;
extern crate peg;

mod token;
mod parser;
mod type_checker;

use std::collections::{HashMap, HashSet};
use std::fs::read_to_string;
use logos::Logos;
use peg::error::ParseError;
use parser::{language, Program};
use token::Token;
use type_checker::TypeChecker;

fn main() {
    if let Ok(source) = read_to_string("input/input.fgo") {
        let tokens: Result<Vec<Token>, ()> = Token::lexer(&source).collect();
        let tokens = tokens.map_err(|_| println!("An error occurred while lexing the source")).unwrap();

        let result: Result<Program, ParseError<usize>> = language::program(&tokens);
        let result = result.map_err(|_| println!("An error occurred while parsing the tokens")).unwrap();

        let mut type_checker = TypeChecker { program: &result, types: HashSet::new(), structs: HashMap::new(), interfaces: HashMap::new() };
        type_checker.check_program();
    }
}