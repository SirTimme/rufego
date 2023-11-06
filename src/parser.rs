use Token;

peg::parser!(
    pub grammar language<'a>() for [Token<'a>] {
        pub rule expr() -> Expr<'a> = precedence!{
            e1:(@) [Token::Plus] e2:@ { Expr::BinOp(Box::new(e1), BinOp::Add, Box::new(e2)) }
            --
            e1:(@) [Token::Star] e2:@ { Expr::BinOp(Box::new(e1), BinOp::Mul, Box::new(e2)) }
            --
            e1:var() [Token::Dot] [Token::LeftParenthesis] e2:var() [Token::RightParenthesis] { Expr::TypeAssertion(Box::new(e1), Box::new(e2)) }
            e1:var() [Token::Dot] e2:var() { Expr::Select(Box::new(e1), Box::new(e2)) }
            --
            v:var() { v }
            n:number() { n }
        }

        rule var() -> Expr<'a>
            = [Token::Identifier(x)] { Expr::Var(x) }

        rule number() -> Expr<'a>
            = [Token::Number(n)] { Expr::Number(n) }
    }
);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr<'a> {
    Number(u64),
    Var(&'a str),                                       // x
    MethodCall,                                         // e.m(e...)
    StructureLiteral,                                   // t{e...}
    Select(Box<Expr<'a>>, Box<Expr<'a>>),                             // e.f
    TypeAssertion(Box<Expr<'a>>, Box<Expr<'a>>),        // e.(t)
    BinOp(Box<Expr<'a>>, BinOp, Box<Expr<'a>>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Mul,
}