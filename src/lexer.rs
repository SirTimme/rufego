use token::Token;

struct Lexer<'a> {
    source: &'a str,
    index: usize,
}

impl Lexer<'_> {
    fn new(source: &str) -> Lexer {
        Lexer { source, index: 0 }
    }

    fn is_at_end(&self) -> bool {
        self.index >= self.source.len()
    }

    fn current(&self) -> char {
        self.source.chars().nth(self.index).unwrap()
    }

    fn advance(&mut self) -> char {
        let token = self.current();
        self.index += 1;
        token
    }

    fn peek(&mut self) -> char {
        if self.index + 1 >= self.source.len() {
            ' '
        } else {
            self.source.chars().nth(self.index + 1).unwrap()
        }
    }
}

pub fn tokenize(source: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut lexer = Lexer::new(source);

    while !lexer.is_at_end() {
        let token = match lexer.current() {
            '(' => Some(Token::LeftParenthesis),
            ')' => Some(Token::RightParenthesis),
            '{' => Some(Token::LeftCurlyBrace),
            '}' => Some(Token::RightCurlyBrace),
            ',' => Some(Token::Comma),
            '-' => Some(Token::Minus),
            '+' => Some(Token::Plus),
            '/' => Some(Token::Slash),
            '*' => Some(Token::Star),
            '.' => Some(Token::Dot),
            _ => None,
        };

        if let Some(token_type) = token {
            tokens.push(token_type);
            lexer.advance();
            continue;
        }

        if lexer.current() == '!' {
            if lexer.peek() == '=' {
                tokens.push(Token::ExclamationMarkEquals);
                lexer.advance();
            } else {
                tokens.push(Token::ExclamationMark)
            }
            lexer.advance();
            continue;
        }

        if lexer.current() == '=' {
            if lexer.peek() == '=' {
                tokens.push(Token::EqualsEquals);
                lexer.advance();
            } else {
                tokens.push(Token::Equals)
            }
            lexer.advance();
            continue;
        }

        if lexer.current() == '<' {
            if lexer.peek() == '=' {
                tokens.push(Token::LessThanEquals);
                lexer.advance();
            } else {
                tokens.push(Token::LessThan)
            }
            lexer.advance();
            continue;
        }

        if lexer.current() == '>' {
            if lexer.peek() == '=' {
                tokens.push(Token::GreaterThanEquals);
                lexer.advance();
            } else {
                tokens.push(Token::GreaterThan)
            }
            lexer.advance();
            continue;
        }

        // ignore newlines
        if lexer.current() == '\'' {
            if lexer.peek() == 'n' {
                lexer.advance();
            }
            lexer.advance();
            continue;
        }

        // ignore whitespace
        if lexer.current().is_whitespace() {
            lexer.advance();
        }
    }

    tokens.push(Token::EndOfFile);

    tokens
}
