use logos::Logos;

#[derive(Debug, Clone, PartialEq, Default)]
pub(crate) enum LexerError {
    #[default]
    InvalidToken
}

impl From<String> for LexerError {
    fn from(_: String) -> Self {
        LexerError::InvalidToken
    }
}

#[derive(Logos, Debug, Copy, Clone)]
#[logos(error = LexerError, skip r"[ \r\t\n\f]+")]
pub(crate) enum Token<'a> {
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
    #[regex("[a-zA-Z][a-zA-Z0-9<>]*", |lex| lex.slice())]
    Identifier(&'a str),
    #[regex("[0-9]+", |lex| lex.slice().parse().ok())]
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
    #[token("var")]
    Var,
    #[token("return")]
    Return,
    #[token("main")]
    Main,
    #[token("int")]
    Int,
}