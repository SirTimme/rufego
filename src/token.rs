#[derive(Debug)]
pub(crate) enum TokenType {
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
    Semicolon,

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
    Number,
    String,

    // keywords
    Function,
    Interface,
    Struct,
    Package,
    Type,
    Var,
    Return,

    // special
    EndOfFile
}

#[derive(Debug)]
pub(crate) struct TokenInfo<'a> {
    pub(crate) source: &'a str,
    pub(crate) length: usize,
    pub(crate) start: usize,
}

#[derive(Debug)]
pub(crate) struct Token<'a> {
    pub(crate) token_type: TokenType,
    pub(crate) token_info: TokenInfo<'a>,
}