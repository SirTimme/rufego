use Token;

peg::parser!(
    pub grammar language<'a>() for [Token<'a>] {
        use Token::*;

        pub rule program() -> Program
            = [Package] [Main] [Semicolon] decl:declaration()* [Function] [Main] [LeftParenthesis] [RightParenthesis] [LeftCurlyBrace] [RightCurlyBrace] { Program::Program }

        rule declaration() -> Declaration<'a>
            = [Function] [LeftParenthesis] [Identifier(method_receiver_name)] [Identifier(method_receiver_type)] [RightParenthesis] [Identifier(method_name)] [LeftParenthesis] [RightParenthesis] [Identifier(return_type)] [LeftCurlyBrace] [RightCurlyBrace] { Declaration::Method }

        // pub rule expr() -> Expression = precedence!{
        //     e1:(@) [Token::Plus] e2:@ {
        //         Expression::BinOp(Box::new(e1), BinOp::Add, Box::new(e2))
        //     }
        //     --
        //     e1:(@) [Token::Star] e2:@ {
        //         Expression::BinOp(Box::new(e1), BinOp::Mul, Box::new(e2))
        //     }
        //     --
        //     [Token::Number(number)] {
        //         Expression::Number(number)
        //     }
        //     [Token::LeftParenthesis] expression:expr() [Token::RightParenthesis] {
        //         expression
        //     }
        // }
        //
        // pub rule stmt() -> Statement<'a> = precedence!{
        //     [Var] [Identifier(name)] [Equals] expression:expr() {
        //         Statement::Assignment(name, expression)
        //     }
        //     [Function] [Identifier(name)] [Token::LeftParenthesis] [Token::RightParenthesis] [Token::LeftCurlyBrace] body:([Token::Identifier(variable_name)] [Token::Identifier(variable_type)] { (variable_name, variable_type) } )* [Token::RightCurlyBrace] {
        //         Statement::Function(name, body)
        //     }
        //     [Token::Package] [Token::Identifier(name)] [Token::Semicolon] {
        //         Statement::Package(name)
        //     }
        // }
        //
        // rule type_literal() -> TypeLiteral = precedence!{
        //     [Interface] [LeftParenthesis] body:
        // }
        //
        // rule method_specification()
        //     = []
    }
);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
    Variable,
    MethodCall,
    StructureLiteral,
    Select,
    TypeAssertion,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeLiteral {
    Structure,
    Interface
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Declaration<'a> {
    Type(&'a str),
    Method
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Program {
    Program
}