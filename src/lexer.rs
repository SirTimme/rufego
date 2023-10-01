use token::{Token, TokenInfo, TokenType};

struct Lexer<'a> {
    source: &'a str,
    index: usize
}

impl<'a> Lexer<'a> {
    fn new(source: &str) -> Lexer {
        Lexer { source, index: 0 }
    }

    fn is_end_of_file(&self) -> bool {
        self.index >= self.source.len()
    }

    fn current(&self) -> char {
        if self.is_end_of_file() {
            ' '
        } else {
            self.source.chars().nth(self.index).unwrap()
        }
    }

    fn advance(&mut self) -> char {
        let token = self.current();
        self.index += 1;
        token
    }

    fn peek(&self) -> char {
        if self.index + 1 >= self.source.len() {
            ' '
        } else {
            self.source.chars().nth(self.index + 1).unwrap()
        }
    }

    fn token_info(&self, size: usize) -> TokenInfo<'a> {
        TokenInfo {
            content: self.source,
            size,
            start_index: self.index
        }
    }

    fn token_info_index(&self, size: usize, start_index: usize) -> TokenInfo<'a> {
        TokenInfo {
            content: self.source,
            size,
            start_index
        }
    }
}

pub(crate) fn tokenize(source: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut lexer = Lexer::new(source);

    while !lexer.is_end_of_file() {
        let token_type = match lexer.current() {
            '(' => Some(TokenType::LeftParenthesis),
            ')' => Some(TokenType::RightParenthesis),
            '{' => Some(TokenType::LeftCurlyBrace),
            '}' => Some(TokenType::RightCurlyBrace),
            ',' => Some(TokenType::Comma),
            '-' => Some(TokenType::Minus),
            '+' => Some(TokenType::Plus),
            '/' => Some(TokenType::Slash),
            '*' => Some(TokenType::Star),
            '.' => Some(TokenType::Dot),
            ';' => Some(TokenType::Semicolon),
            _ => None
        };

        if let Some(token_type) = token_type {
            tokens.push(Token {
                token_type,
                token_info: lexer.token_info(1)
            });
            lexer.advance();
            continue;
        }

        if lexer.current() == '!' {
            if lexer.peek() == '=' {
                tokens.push(Token {
                    token_type: TokenType::ExclamationMarkEquals,
                    token_info: lexer.token_info(2)
                });
                lexer.advance();
            } else {
                tokens.push(Token {
                    token_type: TokenType::ExclamationMark,
                    token_info: lexer.token_info(1)
                });
            }
            lexer.advance();
            continue;
        }

        if lexer.current() == '=' {
            if lexer.peek() == '=' {
                tokens.push(Token {
                    token_type: TokenType::EqualsEquals,
                    token_info: lexer.token_info(2)
                });
                lexer.advance();
            } else {
                tokens.push(Token {
                    token_type: TokenType::Equals,
                    token_info: lexer.token_info(1)
                });
            }
            lexer.advance();
            continue;
        }

        if lexer.current() == '<' {
            if lexer.peek() == '=' {
                tokens.push(Token {
                    token_type: TokenType::LessThanEquals,
                    token_info: lexer.token_info(2)
                });
                lexer.advance();
            } else {
                tokens.push(Token {
                    token_type: TokenType::LessThan,
                    token_info: lexer.token_info(1)
                });
            }
            lexer.advance();
            continue;
        }

        if lexer.current() == '>' {
            if lexer.peek() == '=' {
                tokens.push(Token {
                    token_type: TokenType::GreaterThanEquals,
                    token_info: lexer.token_info(2)
                });
                lexer.advance();
            } else {
                tokens.push(Token {
                    token_type: TokenType::GreaterThan,
                    token_info: lexer.token_info(1)
                });
            }
            lexer.advance();
            continue;
        }

        if lexer.current().is_numeric() {
            let start_index = lexer.index;
            lexer.advance();

            while lexer.current().is_numeric() {
                lexer.advance();
            }

            tokens.push(Token {
                token_type: TokenType::Number,
                token_info: lexer.token_info_index(lexer.index - start_index, start_index),
            });
            continue;
        }

        if lexer.current() == '"' {
            let start_index = lexer.index;
            lexer.advance();

            while lexer.current() != '"' && !lexer.is_end_of_file() {
                lexer.advance();
            }

            lexer.advance();

            tokens.push(Token {
                token_type: TokenType::String,
                token_info: lexer.token_info_index(lexer.index - start_index - 2, start_index + 1)
            });
            continue;
        }

        if lexer.current().is_alphanumeric() {
            let start_index = lexer.index;
            lexer.advance();

            while lexer.current().is_alphanumeric() {
                lexer.advance();
            }

            let size = lexer.index - start_index;
            let lexeme = &lexer.source[start_index..][..size];

            let token_type = match lexeme {
                "package" => TokenType::Package,
                "struct" => TokenType::Struct,
                "interface" => TokenType::Interface,
                "func" => TokenType::Function,
                "return" => TokenType::Return,
                "type" => TokenType::Type,
                "var" => TokenType::Var,
                _ => TokenType::Identifier
            };

            tokens.push(Token {
                token_type,
                token_info: lexer.token_info(size)
            });
            continue;
        }

        if lexer.current().is_whitespace() {
            lexer.advance();
        }
    }

    tokens.push(Token {
        token_type: TokenType::EndOfFile,
        token_info: lexer.token_info(0)
    });

    tokens
}