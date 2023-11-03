use logos::Logos;

#[derive(Logos, Debug)]
#[logos(skip r"[ \r\t\n\f]+")]
pub(crate) enum Token {
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
    #[regex("[a-zA-Z][a-zA-Z0-9]*")]
    Identifier,
    #[regex("[0-9]+")]
    Number,
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
    Return
}