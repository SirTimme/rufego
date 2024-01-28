extern crate logos;

use std::collections::HashMap;
use std::fs::read_to_string;
use logos::Logos;
use parser::language::parse;
use parser::Program;
use token::{LexerError, Token};
use type_checker::{build_type_infos, check_program, TypeInfo};

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
        Ok(program) => create_type_infos(&program),
        Err(parser_error) => {
            eprintln!("Parsing failed with the following error: {parser_error}")
        }
    }
}

fn create_type_infos(program: &Program) {
    match build_type_infos(program) {
        Ok(type_infos) => typecheck_program(program, &type_infos),
        Err(type_info_error) => {
            eprintln!("Creating the type infos failed with the following error: {:?}", type_info_error.message);
        }
    }
}

fn typecheck_program(program: &Program, type_infos: &HashMap<&str, TypeInfo>) {
    match check_program(program, type_infos) {
        Ok(type_) => println!("Main expression has type '{:?}'", type_),
        Err(type_error) => {
            eprintln!("Typechecking failed with the following error: {:?}", type_error.message);
        }
    }
}
