extern crate logos;

use std::fs::read_to_string;
use logos::Logos;
use parser::language::parse;
use token::{LexerError, Token};

mod token;
mod parser;
mod type_checker;

fn main() {
    read_input("featherweight_generic_go/input/input.go");
}

fn read_input(filename: &str) {
    match read_to_string(filename) {
        Ok(source) => lex_source(source),
        Err(error) => {
            eprintln!("Reading in the source file failed with the following error: {error}")
        }
    }
}

fn lex_source(source: String) {
    match Token::lexer(&source).collect::<Result<Vec<Token>, LexerError>>() {
        Ok(tokens) => parse_tokens(&tokens),
        Err(lexer_error) => {
            eprintln!("Lexing the source failed with the following error: {:?}", lexer_error)
        }
    }
}

fn parse_tokens(tokens: &[Token]) {
    match parse(tokens) {
        Ok(program) => println!("Current program {:#?}", program),
        Err(parser_error) => {
            eprintln!("Parsing failed with the following error: {parser_error}")
        }
    }
}
