extern crate logos;
extern crate peg;

mod token;
mod parser;
mod type_checker;
mod interpreter;

use std::collections::{HashMap};
use std::fs::read_to_string;
use logos::Logos;
use interpreter::{evaluate};
use parser::language::parse;
use token::{LexerError, Token};
use type_checker::{build_type_infos, check_program};

fn main() {
    if let Ok(source) = read_to_string("input/input.fgo") {
        let token_result: Result<Vec<Token>, LexerError> = Token::lexer(&source).collect();

        match token_result {
            Ok(tokens) => {
                match parse(&tokens) {
                    Ok(program) => {
                        match build_type_infos(&program) {
                            Ok(type_infos) => {
                                match check_program(&program, &type_infos) {
                                    Ok(_) => {
                                        match evaluate(&program.expression, &HashMap::new(), &type_infos) {
                                            Ok(value) => {
                                                eprintln!("Program evaluates to {:?}", value);
                                            }
                                            Err(eval_error) => {
                                                eprintln!("Evaluation of program failed with following error: {:?}", eval_error.message);
                                            }
                                        }
                                    }
                                    Err(type_error) => {
                                        eprintln!("Typechecking failed with the following error: {:?}", type_error.message);
                                    }
                                }
                            }
                            Err(type_info_error) => {
                                eprintln!("Creating the type infos failed with the following error: {:?}", type_info_error.message);
                            }
                        }
                    }
                    Err(parser_error) => {
                        eprintln!("Parsing failed with the following error: {parser_error}")
                    }
                }
            }
            Err(lexer_error) => {
                eprintln!("Lexing the source failed with the following error: {:?}", lexer_error)
            }
        }
    }
}