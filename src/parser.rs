use Token;

peg::parser!(
    pub grammar language<'a>() for [Token<'a>] {
        pub rule expr() -> Expression = precedence!{
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
pub enum Expression {
    Number(u64),
    MethodCall,
    StructureLiteral,
    Select(Box<Expression>, Box<Expression>),
    TypeAssertion(Box<Expression>, Box<Expression>),
    BinOp(Box<Expression>, BinOp, Box<Expression>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement<'a> {
    // name, body
    Function(&'a str, Vec<Statement<'a>>),
    // name, value
    Assignment(&'a str, Expression),
    // name
    Package(&'a str),
    // body
    Program(Vec<Statement<'a>>)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Mul,
}