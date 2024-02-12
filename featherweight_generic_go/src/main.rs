extern crate logos;

use std::collections::HashMap;
use std::fs::read_to_string;
use logos::Logos;
use interpreter::{evaluate, Value};
use monomorpher::monomorph;
use parser::language::parse;
use parser::{Expression, Program};
use token::{LexerError, Token};
use type_checker::{create_type_infos, check_program, TypeInfo, TypeInfos};

mod token;
mod parser;
mod type_checker;
mod interpreter;
mod diagnostics;
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
        Ok(type_infos) => typecheck_program(program, &type_infos),
        Err(error) => eprintln!("{}", error.message),
    }
}

fn typecheck_program(program: &Program, type_infos: &HashMap<&str, TypeInfo>) {
    match check_program(program, type_infos) {
        Ok(_) => evaluate_program(&program.expression, &HashMap::new(), type_infos),
        Err(error) => eprintln!("{}", error.message),
    }
}

fn evaluate_program(expression: &Expression, context: &HashMap<&str, Value>, type_infos: &HashMap<&str, TypeInfo>) {
    match evaluate(expression, context, type_infos) {
        Ok(value) => {
            println!("Result: {:?}", value);
            monomorph_program(expression, type_infos);
        },
        Err(error) => eprintln!("{}", error.message),
    }
}

fn monomorph_program(expression: &Expression, type_infos: &TypeInfos) {
    if monomorph(expression, type_infos).is_ok() {}
}
