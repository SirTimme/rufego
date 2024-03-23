use std::fmt::Write;
use crate::token::Token;

peg::parser!(
    pub grammar language<'a>() for [Token<'a>] {
        use crate::token::Token::*;

        pub rule parse() -> Program<'a>
            = [Package] [Main] [Semicolon] declarations:declaration()* [Function] [Main] [LeftParenthesis] [RightParenthesis] [LeftCurlyBrace] [Underscore] [Equals] expression:expression() [RightCurlyBrace] {
                Program { declarations, expression: Box::new(expression) }
            }

        rule declaration() -> Declaration<'a>
            = [Type] [Identifier(name)] [LeftParenthesis] bound:formal_type() [RightParenthesis] literal:type_literal() { Declaration::Type { name, bound, literal } }
            / [Function] [LeftParenthesis] receiver:generic_receiver() [RightParenthesis] specification:method_specification() [LeftCurlyBrace] [Return] body:expression() [RightCurlyBrace] {
                Declaration::Method(MethodDeclaration { receiver, specification, body })
            }

        rule type_literal() -> TypeLiteral<'a>
            = [Struct] [LeftCurlyBrace] fields:generic_binding()* [RightCurlyBrace] { TypeLiteral::Struct { fields } }
            / [Interface] [LeftCurlyBrace] methods:method_specification()* [RightCurlyBrace] { TypeLiteral::Interface { methods } }

        rule method_specification() -> MethodSpecification<'a>
            = [Identifier(name)] [LeftParenthesis] bound:formal_type() [RightParenthesis] [LeftParenthesis] parameters:(generic_binding() ** [Comma]) [RightParenthesis] return_type:generic_type() {
                MethodSpecification { name, bound, parameters, return_type }
            }

        rule formal_type() -> Vec<GenericBinding<'a>>
            = [Type] generic_params:(generic_binding() ** [Comma]) { generic_params }

        rule generic_binding() -> GenericBinding<'a>
            = [Identifier(name)] type_:generic_type() { GenericBinding { name, type_ } }

        rule generic_type() -> GenericType<'a>
            = [Identifier(name)] [LeftParenthesis] values:(generic_type() ** [Comma]) [RightParenthesis] { GenericType::NamedType(name, values) }
            / [Identifier(name)] { GenericType::TypeParameter(name) }
            / [Int] { GenericType::NumberType }

        rule generic_receiver() -> GenericReceiver<'a>
            = [Identifier(receiver_var)] [Identifier(receiver_type)] [LeftParenthesis] generic_params:formal_type() [RightParenthesis] { GenericReceiver { name: receiver_var, type_: receiver_type, instantiation: generic_params } }

        #[cache]
        rule expression() -> Expression<'a> = precedence!{
            lhs:(@) [Plus] rhs:@ {
                Expression::BinOp { lhs: Box::new(lhs), operator: Operator::Add, rhs: Box::new(rhs) }
            }
            --
            lhs:(@) [Star] rhs:@ { Expression::BinOp {
                lhs: Box::new(lhs), operator: Operator::Mul, rhs: Box::new(rhs) }
            }
            --
            expression:(@) [Dot] [LeftParenthesis] assert:generic_type() [RightParenthesis] {
                Expression::TypeAssertion { expression: Box::new(expression), assert }
            }
            expression:(@) [Dot] [Identifier(method)] [LeftParenthesis] instantiation:(generic_type() ** [Comma]) [RightParenthesis] [LeftParenthesis] parameter_expressions:(expression() ** [Comma]) [RightParenthesis] {
                Expression::MethodCall { expression: Box::new(expression), method, instantiation, parameter_expressions }
            }
            expression:(@) [Dot] [Identifier(field)] {
                Expression::Select { expression: Box::new(expression), field }
            }
            --
            [Identifier(name)] [LeftParenthesis] instantiation:(generic_type() ** [Comma]) [RightParenthesis] [LeftCurlyBrace] field_expressions:(expression() ** [Comma]) [RightCurlyBrace] {
                Expression::StructLiteral { name, instantiation, field_expressions }
            }
            [Identifier(name)] {
                Expression::Variable { name }
            }
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
    pub declarations: Vec<Declaration<'a>>,
    pub expression: Box<Expression<'a>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Declaration<'a> {
    Type {
        name: &'a str,
        bound: Vec<GenericBinding<'a>>,
        literal: TypeLiteral<'a>,
    },
    Method(MethodDeclaration<'a>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MethodDeclaration<'a> {
    pub receiver: GenericReceiver<'a>,
    pub specification: MethodSpecification<'a>,
    pub body: Expression<'a>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeLiteral<'a> {
    Struct {
        fields: Vec<GenericBinding<'a>>
    },
    Interface {
        methods: Vec<MethodSpecification<'a>>
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum GenericType<'a> {
    TypeParameter(&'a str),
    NamedType(&'a str, Vec<GenericType<'a>>),
    NumberType,
}

impl<'a> GenericType<'a> {
    pub fn name(&self) -> &'a str {
        match self {
            GenericType::TypeParameter(type_parameter) => type_parameter,
            GenericType::NamedType(name, _) => name,
            GenericType::NumberType => "int",
        }
    }

    pub fn close_type(&self) -> String {
        match self {
            GenericType::TypeParameter(type_parameter) => {
                String::from(*type_parameter)
            }
            GenericType::NamedType(name, instantiation) => {
                let mut result_string = String::new();

                write!(&mut result_string, "{name}<").unwrap();

                for (index, instantiated_type) in instantiation.iter().enumerate() {
                    let monomorphed_type = instantiated_type.close_type();

                    write!(&mut result_string, "{monomorphed_type}").unwrap();

                    if index < instantiation.len() - 1 {
                        write!(&mut result_string, ",").unwrap();
                    }
                }
        
                write!(&mut result_string, ">").unwrap();

                result_string
            }
            GenericType::NumberType => {
                String::from("int")
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenericBinding<'a> {
    pub name: &'a str,
    pub type_: GenericType<'a>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GenericReceiver<'a> {
    pub name: &'a str,
    pub type_: &'a str,
    pub instantiation: Vec<GenericBinding<'a>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression<'a> {
    Variable {
        name: &'a str
    },
    MethodCall {
        expression: Box<Expression<'a>>,
        method: &'a str,
        instantiation: Vec<GenericType<'a>>,
        parameter_expressions: Vec<Expression<'a>>,
    },
    StructLiteral {
        name: &'a str,
        instantiation: Vec<GenericType<'a>>,
        field_expressions: Vec<Expression<'a>>,
    },
    Select {
        expression: Box<Expression<'a>>,
        field: &'a str,
    },
    TypeAssertion {
        expression: Box<Expression<'a>>,
        assert: GenericType<'a>,
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

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct MethodSpecification<'a> {
    pub name: &'a str,
    pub bound: Vec<GenericBinding<'a>>,
    pub parameters: Vec<GenericBinding<'a>>,
    pub return_type: GenericType<'a>,
}

#[derive(Debug)]
pub struct RufegoError {
    pub message: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator {
    Add,
    Mul,
}

impl Operator {
    pub fn as_str(&self) -> String {
        match self {
            Operator::Add => {
                String::from("+")
            }
            Operator::Mul => {
                String::from("*")
            }
        }
    }
}