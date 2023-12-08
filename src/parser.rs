use Token;

peg::parser!(
    pub grammar language<'a>() for [Token<'a>] {
        use Token::*;

        pub rule program() -> Program<'a>
            = [Package] [Main] [Semicolon] decl:declaration()* [Function] [Main] [LeftParenthesis] [RightParenthesis] [LeftCurlyBrace] [RightCurlyBrace] { Program { declarations: decl } }

        rule declaration() -> Declaration<'a>
            = [Function] [LeftParenthesis] receiver:bound() [RightParenthesis] [Identifier(method_name)] [LeftParenthesis] params:bound()* [RightParenthesis] [Identifier(return_type)] [LeftCurlyBrace] [Return] body:expression()  [RightCurlyBrace] { Declaration::Method(receiver, method_name, params, return_type, body) }
            / literal:type_literal() { Declaration::Type(literal) }

        rule type_literal() -> TypeLiteral<'a>
            = [Type] [Identifier(struct_name)] [Struct] [LeftCurlyBrace] fields:bound()* [RightCurlyBrace] { TypeLiteral::Structure(struct_name, fields) }
            / [Type] [Identifier(interface_name)] [Interface] [LeftCurlyBrace] methods:method_body()* [RightCurlyBrace] { TypeLiteral::Interface(interface_name, methods) }

        rule bound() -> Binding<'a>
            = [Identifier(variable)] [Identifier(type_)] [Comma]? { Binding { variable, type_ }}

        rule method_body() -> MethodBody<'a>
            = [Identifier(method_name)] [LeftParenthesis] params:bound()* [RightParenthesis] [Identifier(return_type)] { MethodBody { name: method_name, params, return_type } }

        rule expression() -> Expression<'a> = precedence!{
            e1:(@) [Plus] e2:@ { Expression::BinOp(Box::new(e1), BinOp::Add, Box::new(e2)) }
            --
            e1:(@) [Star] e2:@ { Expression::BinOp(Box::new(e1), BinOp::Mul, Box::new(e2)) }
            --
            [Number(number)] { Expression::Number(number) }
            [LeftParenthesis] expr:expression() [RightParenthesis] { expr }
        }
    }
);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program<'a> {
    declarations: Vec<Declaration<'a>>
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Declaration<'a> {
    Type(TypeLiteral<'a>),
    Method(Binding<'a>, &'a str, Vec<Binding<'a>>, &'a str, Expression<'a>)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Binding<'a> {
    variable: &'a str,
    type_: &'a str
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression<'a> {
    Variable(&'a str),
    MethodCall,
    StructureLiteral,
    Select,
    TypeAssertion,
    BinOp(Box<Expression<'a>>, BinOp, Box<Expression<'a>>),
    Number(u64)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Mul,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeLiteral<'a> {
    Structure(&'a str, Vec<Binding<'a>>),
    Interface(&'a str, Vec<MethodBody<'a>>)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MethodBody<'a> {
    name: &'a str,
    params: Vec<Binding<'a>>,
    return_type: &'a str
}