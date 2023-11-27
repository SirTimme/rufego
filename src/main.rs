extern crate logos;
extern crate peg;

mod token;
mod parser;

use std::fs::read_to_string;
use logos::Logos;
use peg::error::ParseError;
use parser::language;
use token::Token;

fn main() {
    if let Ok(source) = read_to_string("input.fgo") {
        let tokens: Result<Vec<Token>, ()> = Token::lexer(&source).collect();
        let tokens = tokens.map_err(|_| format!("An error occurred while lexing the source")).unwrap();

        let result: Result<parser::Program, ParseError<usize>> = language::program(&tokens);
        let result = result.map_err(|_| format!("An error occurred while parsing the tokens")).unwrap();

        println!("{:?}", result);
    }
}