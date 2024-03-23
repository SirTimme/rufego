use std::fs::read_to_string;
use common::create_type_infos;
use common::parser::language::parse;
use common::token::lex_program;
use crate::monomorpher::monomorph_program;

mod monomorpher;

fn main() {
    let source_file = match read_to_string("monomorpher/input/input.go") {
        Ok(source) => source,
        Err(error) => {
            eprintln!("Reading the source file failed: {error}");
            return;
        }
    };

    let tokens = match lex_program(source_file.as_str()) {
        Ok(tokens) => tokens,
        Err(error) => {
            eprintln!("{}", error.message);
            return;
        }
    };

    let program = match parse(&tokens) {
        Ok(program) => program,
        Err(error) => {
            eprintln!("Parsing the program failed: {error}");
            return;
        }
    };

    let type_infos = match create_type_infos(&program) {
        Ok(type_infos) => type_infos,
        Err(error) => {
            eprintln!("Creating the type infos failed: {}", error.message);
            return;
        }
    };

    match monomorph_program(&program, &type_infos) {
        Ok(_) => {}
        Err(error) => {
            eprintln!("Error: {}", error.message)
        }
    }
}
