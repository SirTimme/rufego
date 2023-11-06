extern crate logos;
extern crate peg;

mod token;
mod parser;

use std::fs::read_to_string;
use logos::Logos;
use parser::language;
use token::Token;

fn main() {
    if let Ok(source) = read_to_string("input.fgo") {
        let tokens: Result<Vec<Token>, ()> = Token::lexer(&source).collect();
        let tokens = tokens.map_err(|_| format!("Lexer failed!!!")).unwrap();
        let result = language::expr(&tokens);

        println!("{:?}", result);
    }
}