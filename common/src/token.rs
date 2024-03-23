use logos::Logos;
use crate::parser::RufegoError;

#[derive(Logos, Debug, Copy, Clone)]
#[logos(skip r"[ \r\t\n\f]+")]
pub enum Token<'a> {
    #[token("(")]
    LeftParenthesis,
    #[token(")")]
    RightParenthesis,
    #[token("{")]
    LeftCurlyBrace,
    #[token("}")]
    RightCurlyBrace,
    #[token(",")]
    Comma,
    #[token("+")]
    Plus,
    #[token("*")]
    Star,
    #[token(".")]
    Dot,
    #[token(";")]
    Semicolon,
    #[token("_")]
    Underscore,
    #[token("=")]
    Equals,
    #[regex("[a-zA-Z][a-zA-Z0-9<>,]*", | lex | lex.slice())]
    Identifier(&'a str),
    #[regex("[0-9]+", | lex | lex.slice().parse().ok())]
    Number(i64),
    #[token("func")]
    Function,
    #[token("interface")]
    Interface,
    #[token("struct")]
    Struct,
    #[token("package")]
    Package,
    #[token("type")]
    Type,
    #[token("return")]
    Return,
    #[token("main")]
    Main,
    #[token("int")]
    Int,
}

pub fn lex_program(source: &str) -> Result<Vec<Token>, RufegoError> {
    match Token::lexer(source).collect::<Result<Vec<Token>, ()>>() {
        Ok(tokens) => Ok(tokens),
        Err(_) => Err(RufegoError { message: String::from("Lexing the source failed")}),
    }
}