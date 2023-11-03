extern crate logos;

mod token;

use std::fs::read_to_string;
use logos::Logos;
use token::Token;

fn main() {
    if let Ok(source) = read_to_string("input.fgo") {
        let tokens: Vec<_> = Token::lexer(&source).collect();

        println!("{:?}", tokens);
    }
}