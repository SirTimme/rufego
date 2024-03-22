use std::collections::HashMap;
use Token;
use parser::Type as TypeEnum;

peg::parser!(
    pub(crate) grammar language<'a>() for [Token<'a>] {
        use Token::*;

        pub(crate) rule parse_program() -> Program<'a>
            = [Package] [Main] [Semicolon] declarations:declaration()* [Function] [Main] [LeftParenthesis] [RightParenthesis] [LeftCurlyBrace] [Underscore] [Equals] body:expression() [RightCurlyBrace] {
                Program { declarations, expression: Box::new(body) }
            }

        rule declaration() -> Declaration<'a>
            = [Function] [LeftParenthesis] receiver:binding_string() [RightParenthesis] specification:method_specification() [LeftCurlyBrace] [Return] body:expression() [RightCurlyBrace] {
                Declaration::Method(MethodDeclaration { receiver, specification, body })
            }
            / [Type] [Identifier(name)] literal:type_literal() { Declaration::Type { name, literal } }

        rule type_literal() -> TypeLiteral<'a>
            = [Struct] [LeftCurlyBrace] fields:binding_type()* [RightCurlyBrace] { TypeLiteral::Struct { fields } }
            / [Interface] [LeftCurlyBrace] methods:method_specification()* [RightCurlyBrace] { TypeLiteral::Interface { methods } }

        rule binding_string() -> Binding<'a, &'a str>
            = [Identifier(name)] [Identifier(type_)] [Comma]? { Binding { name, type_ } }

        rule binding_type() -> Binding<'a, TypeEnum<'a>>
            = [Identifier(name)] type_:type_() [Comma]? { Binding { name, type_ } }

        rule method_specification() -> MethodSpecification<'a>
            = [Identifier(name)] [LeftParenthesis] parameters:binding_type()* [RightParenthesis] return_type:type_() { MethodSpecification { name, parameters, return_type } }

        rule type_() -> TypeEnum<'a>
            = [Int] { TypeEnum::Int }
            / [Identifier(name)] { TypeEnum::Struct(name) }

        rule expression() -> Expression<'a> = precedence!{
            lhs:(@) [Plus] rhs:@ { Expression::BinOp { lhs: Box::new(lhs), operator: Operator::Add, rhs: Box::new(rhs) } }
            --
            lhs:(@) [Star] rhs:@ { Expression::BinOp { lhs: Box::new(lhs), operator: Operator::Mul, rhs: Box::new(rhs) } }
            --
            expression:(@) [Dot] [LeftParenthesis] assert:type_() [RightParenthesis] { Expression::TypeAssertion { expression: Box::new(expression), assert } }
            expression:(@) [Dot] [Identifier(method)] [LeftParenthesis] parameter_expressions:(expression() ** [Comma]) [RightParenthesis] {
                Expression::MethodCall { expression: Box::new(expression), method, parameter_expressions }
            }
            expression:(@) [Dot] [Identifier(field)] { Expression::Select { expression: Box::new(expression), field } }
            --
            [Identifier(name)] [LeftCurlyBrace] field_expressions:(expression() ** [Comma]) [RightCurlyBrace] { Expression::StructLiteral { name, field_expressions } }
            [Identifier(name)] { Expression::Variable { name } }
            [Number(value)] { Expression::Number { value } }
            [LeftParenthesis] expression:expression() [RightParenthesis] { expression }
        }
    }
);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program<'a> {
    pub declarations: Vec<Declaration<'a>>,
    pub expression: Box<Expression<'a>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Declaration<'a> {
    Type {
        name: &'a str,
        literal: TypeLiteral<'a>,
    },
    Method(MethodDeclaration<'a>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MethodDeclaration<'a> {
    pub receiver: Binding<'a, &'a str>,
    pub specification: MethodSpecification<'a>,
    pub body: Expression<'a>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeLiteral<'a> {
    Struct {
        fields: Vec<Binding<'a, Type<'a>>>
    },
    Interface {
        methods: Vec<MethodSpecification<'a>>
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Binding<'a, T> {
    pub name: &'a str,
    pub type_: T,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression<'a> {
    Variable {
        name: &'a str
    },
    MethodCall {
        expression: Box<Expression<'a>>,
        method: &'a str,
        parameter_expressions: Vec<Expression<'a>>,
    },
    StructLiteral {
        name: &'a str,
        field_expressions: Vec<Expression<'a>>,
    },
    Select {
        expression: Box<Expression<'a>>,
        field: &'a str,
    },
    TypeAssertion {
        expression: Box<Expression<'a>>,
        assert: Type<'a>,
    },
    Number {
        value: i64
    },
    BinOp {
        lhs: Box<Expression<'a>>,
        operator: Operator,
        rhs: Box<Expression<'a>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MethodSpecification<'a> {
    pub name: &'a str,
    pub parameters: Vec<Binding<'a, Type<'a>>>,
    pub return_type: Type<'a>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator {
    Add,
    Mul,
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum Type<'a> {
    Int,
    Struct(&'a str),
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum TypeInfo<'a> {
    Struct(&'a Vec<Binding<'a, Type<'a>>>, HashMap<&'a str, &'a MethodDeclaration<'a>>),
    Interface(&'a Vec<MethodSpecification<'a>>),
}

impl<'a> TypeInfo<'a> {
    pub fn method_spec(&self, method_name: &'a str) -> Option<&'a MethodSpecification<'a>> {
        match self {
            TypeInfo::Struct(.., methods) => {
                methods.get(method_name).map(|method| &method.specification)
            }
            TypeInfo::Interface(methods) => {
                methods.iter().find(|method| method.name == method_name)
            }
        }
    }
}

#[derive(Debug)]
pub struct RufegoError {
    pub message: String,
}
