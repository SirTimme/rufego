mod lexer;
mod token;

use std::fs::read_to_string;

fn main() {
    if let Ok(source) = read_to_string("input.fgo") {
        run(&source);
    }
}

fn run(source: &str) {
    for token in lexer::tokenize(source) {
        println!("{:?}", token)
    }
}
