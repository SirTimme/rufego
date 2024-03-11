extern crate logos;
extern crate peg;

mod token;
mod parser;
mod type_checker;
mod interpreter;

use std::collections::{HashMap};
use std::fs::read_to_string;
use logos::Logos;
use interpreter::{evaluate_expression, Value};
use parser::{Expression, Program, TypeInfo};
use parser::language::parse;
use token::{LexerError, Token};
use type_checker::{build_type_infos, check_program};

fn main() {
    read_input("output/output.go");
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
        Err(error) => {
            eprintln!("Parsing the program failed: {error}")
        }
    }
}

fn create_type_infos(program: &Program) {
    match build_type_infos(program) {
        Ok(type_infos) => typecheck_program(program, &type_infos),
        Err(error) => {
            eprintln!("Creating the type infos failed with the following error: {}", error.message);
        }
    }
}

fn typecheck_program(program: &Program, type_infos: &HashMap<&str, TypeInfo>) {
    match check_program(program, type_infos) {
        Ok(_) => evaluate_program(&program.expression, &HashMap::new(), type_infos),
        Err(error) => {
            eprintln!("Program is not well-formed: {}", error.message);
        }
    }
}

fn evaluate_program(expression: &Expression, context: &HashMap<&str, Value>, types: &HashMap<&str, TypeInfo>) {
    match evaluate_expression(expression, context, types) {
        Ok(value) => {
            println!("{:?}", value);
        }
        Err(error) => {
            eprintln!("Evaluation of program failed: {}", error.message);
        }
    }
}
