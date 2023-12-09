use Token;

peg::parser!(
    pub grammar language<'a>() for [Token<'a>] {
        use Token::*;

        pub rule program() -> Program<'a>
            = [Package] [Main] [Semicolon] declarations:declaration()* [Function] [Main] [LeftParenthesis] [RightParenthesis] [LeftCurlyBrace] [RightCurlyBrace] {
                Program { declarations }
            }

        rule declaration() -> Declaration<'a>
            = [Function] [LeftParenthesis] receiver:binding() [RightParenthesis] [Identifier(name)] [LeftParenthesis] parameters:binding()* [RightParenthesis] [Identifier(return_type)] [LeftCurlyBrace] [Return] body:expression() [RightCurlyBrace] {
                Declaration::Method { receiver, name, parameters, return_type, body }
            }
            / literal:type_literal() {
                Declaration::Type { literal }
            }

        rule type_literal() -> TypeLiteral<'a>
            = [Type] [Identifier(name)] [Struct] [LeftCurlyBrace] fields:binding()* [RightCurlyBrace] {
                TypeLiteral::Struct { name, fields }
            }
            / [Type] [Identifier(name)] [Interface] [LeftCurlyBrace] methods:method_body()* [RightCurlyBrace] {
                TypeLiteral::Interface { name, methods }
            }

        rule binding() -> Binding<'a>
            = [Identifier(variable)] [Identifier(type_)] [Comma]? {
                Binding { variable, type_ }
            }

        rule method_body() -> MethodBody<'a>
            = [Identifier(name)] [LeftParenthesis] params:binding()* [RightParenthesis] [Identifier(return_type)] {
                MethodBody { name, params, return_type }
            }

        rule expression() -> Expression<'a>
            = [Identifier(variable)] [Dot] [LeftParenthesis] [Identifier(assertion)] [RightParenthesis] {
                Expression::TypeAssertion { variable, assertion }
            }
            / [Identifier(variable)] [Dot] [Identifier(method)] [LeftParenthesis] parameters:binding()* [RightParenthesis] {
                Expression::MethodCall { variable, method, parameters }
            }
            / [Identifier(name)] [Dot] [Identifier(field)] {
                Expression::Select { name, field }
            }
            / [Identifier(name)] [LeftCurlyBrace] fields:(expr:expression() [Comma]? { expr })* [RightCurlyBrace] {
                Expression::StructLiteral { name, fields }
            }
            / [Identifier(name)] {
                Expression::Variable { name }
            }
            / expression:arithmetic() {
                expression
            }

        rule arithmetic() -> Expression<'a> = precedence!{
            lhs:(@) [Plus] rhs:@ {
                Expression::BinOp { lhs: Box::new(lhs), operator: Operator::Add, rhs: Box::new(rhs) }
            }
            --
            lhs:(@) [Star] rhs:@ { Expression::BinOp {
                lhs: Box::new(lhs), operator: Operator::Mul, rhs: Box::new(rhs) }
            }
            --
            [Number(value)] {
                Expression::Number { value }
            }
            [LeftParenthesis] expression:expression() [RightParenthesis] {
                expression
            }
        }
    }
);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program<'a> {
    declarations: Vec<Declaration<'a>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Declaration<'a> {
    Type {
        literal: TypeLiteral<'a>
    },
    Method {
        receiver: Binding<'a>,
        name: &'a str,
        parameters: Vec<Binding<'a>>,
        return_type: &'a str,
        body: Expression<'a>
    }
    // Type(TypeLiteral<'a>),
    // Method(Binding<'a>, &'a str, Vec<Binding<'a>>, &'a str, Expression<'a>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeLiteral<'a> {
    Struct {
        name: &'a str,
        fields: Vec<Binding<'a>>
    },
    Interface {
        name: &'a str,
        methods: Vec<MethodBody<'a>>
    }
    // Structure(&'a str, Vec<Binding<'a>>),
    // Interface(&'a str, Vec<MethodBody<'a>>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Binding<'a> {
    variable: &'a str,
    type_: &'a str,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression<'a> {
    Variable {
        name: &'a str
    },
    MethodCall {
        variable: &'a str,
        method: &'a str,
        parameters: Vec<Binding<'a>>
    },
    StructLiteral {
        name: &'a str,
        fields: Vec<Expression<'a>>
    },
    Select {
        name: &'a str,
        field: &'a str
    },
    TypeAssertion {
        variable: &'a str,
        assertion: &'a str
    },
    Number {
        value: u64
    },
    BinOp {
        lhs: Box<Expression<'a>>,
        operator: Operator,
        rhs: Box<Expression<'a>>
    }
    // Variable(&'a str),
    // MethodCall(&'a str, &'a str),
    // StructureLiteral(&'a str, Vec<Expression<'a>>),
    // Select(&'a str, &'a str),
    // TypeAssertion(&'a str, &'a str),
    // BinOp(Box<Expression<'a>>, BinOp, Box<Expression<'a>>),
    // Number(u64),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MethodBody<'a> {
    name: &'a str,
    params: Vec<Binding<'a>>,
    return_type: &'a str,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator {
    Add,
    Mul,
}