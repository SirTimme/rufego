extern crate logos;
extern crate common;

use std::collections::HashMap;
use std::fs::read_to_string;
use logos::Logos;
use common::RufegoError;
use interpreter::{evaluate, Value};
use monomorpher::monomorph_program;
use parser::language::parse;
use parser::{Expression, Program};
use token::{LexerError, Token};
use type_checker::{create_type_infos, program_well_formed, TypeInfo, TypeInfos};

mod token;
mod parser;
mod type_checker;
mod interpreter;
mod monomorpher;

fn main() {
    read_input("featherweight_generic_go/input/input.go");
}

fn read_input(filename: &str) {
    match read_to_string(filename) {
        Ok(source) => lex_source(source),
        Err(error) => eprintln!("{error}"),
    }
}

fn lex_source(source: String) {
    match Token::lexer(&source).collect::<Result<Vec<Token>, LexerError>>() {
        Ok(tokens) => parse_tokens(&tokens),
        Err(lexer_error) => eprintln!("{:?}", lexer_error),
    }
}

fn parse_tokens(tokens: &[Token]) {
    match parse(tokens) {
        Ok(program) => build_type_infos(&program),
        Err(error) => eprintln!("{error}"),
    }
}

fn build_type_infos(program: &Program) {
    match create_type_infos(program) {
        Ok(type_infos) => check_program(program, &type_infos),
        Err(error) => eprintln!("{}", error.message),
    }
}

fn check_program(program: &Program, type_infos: &HashMap<&str, TypeInfo>) {
    match program_well_formed(program, type_infos) {
        Ok(_) => {
            run_program(&program.expression, &HashMap::new(), type_infos);
            monomorph(program, type_infos);
            
        },
        Err(error) => eprintln!("Program is not well-formed:\n{}", error.message),
    }
}

fn run_program(expression: &Expression, context: &HashMap<&str, Value>, type_infos: &HashMap<&str, TypeInfo>) {
    match evaluate(expression, context, type_infos) {
        Ok(value) => println!("{:#?}", value),
        Err(error) => eprintln!("Runtime evaluation of expression failed:\n{}", error.message),
    }
}

fn monomorph(program: &Program, type_infos: &TypeInfos) {
    match monomorph_program(program, type_infos) {
        Ok(_) => {}
        Err(error) => {
            eprintln!("Error: {}", error.message)
        }
    }
}
