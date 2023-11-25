use Token;

peg::parser!(
    pub grammar language<'a>() for [Token<'a>] {
        pub rule expr() -> Expression<'a> = precedence!{
            e1:(@) [Token::Plus] e2:@ {
                Expression::BinOp(Box::new(e1), BinOp::Add, Box::new(e2))
            }
            --
            e1:(@) [Token::Star] e2:@ {
                Expression::BinOp(Box::new(e1), BinOp::Mul, Box::new(e2))
            }
            --
            [Token::Number(number)] {
                Expression::Number(number)
            }
            [Token::LeftParenthesis] expression:expr() [Token::RightParenthesis] {
                expression
            }
        }

        pub rule stmt() -> Statement<'a> = precedence!{
            [Token::Var] [Token::Identifier(name)] [Token::Equals] expression:expr() {
                Statement::Assignment(name, expression)
            }
            [Token::Function] [Token::Identifier(name)] [Token::LeftParenthesis] [Token::RightParenthesis] [Token::LeftCurlyBrace] body:stmt()* [Token::RightCurlyBrace] {
                Statement::Function(name, body)
            }
            [Token::Package] [Token::Identifier(name)] [Token::Semicolon] {
                Statement::Package(name)
            }
        }
    }
);

pub enum Node {
    Expression,
    Statement
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression<'a> {
    Number(u64),
    MethodCall,                                         // e.m(e...)
    StructureLiteral,                                   // t{e...}
    Select(Box<Expression<'a>>, Box<Expression<'a>>),               // e.f
    TypeAssertion(Box<Expression<'a>>, Box<Expression<'a>>),        // e.(t)
    BinOp(Box<Expression<'a>>, BinOp, Box<Expression<'a>>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement<'a> {
    // name, body
    Function(&'a str, Vec<Statement<'a>>),
    // name
    Package(&'a str),
    // name, value
    Assignment(&'a str, Expression<'a>)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Mul,
}