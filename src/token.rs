use logos::Logos;

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
    #[token("-")]
    Minus,
    #[token("+")]
    Plus,
    #[token("/")]
    Slash,
    #[token("*")]
    Star,
    #[token(".")]
    Dot,
    #[token(";")]
    Semicolon,
    #[token("_")]
    Underscore,
    #[token("!")]
    ExclamationMark,
    #[token("!=")]
    ExclamationMarkEquals,
    #[token("=")]
    Equals,
    #[token("==")]
    EqualsEquals,
    #[token(">=")]
    GreaterThanEquals,
    #[token("<=")]
    LessThanEquals,
    #[token(">")]
    GreaterThan,
    #[token("<")]
    LessThan,
    #[regex("[a-zA-Z][a-zA-Z0-9]*", |lex| lex.slice())]
    Identifier(&'a str),
    #[regex("[0-9]+", |lex| lex.slice().parse().ok())]
    Number(i64),
    #[regex("\"[a-zA-Z0-9 ]+\"")]
    String,
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