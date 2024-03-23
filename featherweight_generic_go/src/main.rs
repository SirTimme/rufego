extern crate common;

use std::collections::HashMap;
use std::fs::read_to_string;
use common::parser::language::parse;
use common::parser::{Expression, Program};
use common::token::{lex_program, Token};
use common::{create_type_infos, TypeInfo};
use interpreter::{evaluate, Value};
use type_checker::{program_well_formed};

mod type_checker;
mod interpreter;

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
    match lex_program(source.as_str()) {
        Ok(tokens) => parse_tokens(&tokens),
        Err(lexer_error) => eprintln!("{}", lexer_error.message),
    }
}

fn parse_tokens(tokens: &[Token]) {
    match parse(tokens) {
        Ok(program) => {
            build_type_infos(&program)
        },
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
