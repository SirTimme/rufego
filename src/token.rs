use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum Token {
    // single-char token
    LeftParenthesis,
    RightParenthesis,
    LeftCurlyBrace,
    RightCurlyBrace,
    Comma,
    Minus,
    Plus,
    Slash,
    Star,
    Dot,

    // one or two char token
    ExclamationMark,
    ExclamationMarkEquals,
    Equals,
    EqualsEquals,
    GreaterThanEquals,
    LessThanEquals,
    GreaterThan,
    LessThan,

    // literals
    Identifier,
    String,
    Number,

    // keywords
    Function,
    Interface,
    Struct,
    Package,
    Type,
    Var,
    Return,

    // special
    EndOfFile,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
