use std::collections::HashMap;
use Token;

peg::parser!(
    pub(crate) grammar language<'a>() for [Token<'a>] {
        use Token::*;

        pub(crate) rule parse() -> FGProgram<'a>
            = [Package] [Main] [Semicolon] declarations:declaration()* [Function] [Main] [LeftParenthesis] [RightParenthesis] [LeftCurlyBrace] [Underscore] [Equals] body:expression() [RightCurlyBrace] {
                FGProgram { declarations, expression: Box::new(body) }
            }

        rule declaration() -> FGDeclaration<'a>
            = [Function] [LeftParenthesis] receiver:binding_string() [RightParenthesis] specification:method_specification() [LeftCurlyBrace] [Return] body:expression() [RightCurlyBrace] {
                FGDeclaration::Method(FGMethodDeclaration { receiver, specification, body })
            }
            / [Type] [Identifier(name)] literal:type_literal() { FGDeclaration::Type { name, literal } }

        rule type_literal() -> FGTypeLiteral<'a>
            = [Struct] [LeftCurlyBrace] fields:binding_type()* [RightCurlyBrace] { FGTypeLiteral::Struct { fields } }
            / [Interface] [LeftCurlyBrace] methods:method_specification()* [RightCurlyBrace] { FGTypeLiteral::Interface { methods } }

        rule binding_string() -> FGBinding<'a, &'a str>
            = [Identifier(name)] [Identifier(type_)] [Comma]? { FGBinding { name, type_ } }

        rule binding_type() -> FGBinding<'a, FGType<'a>>
            = [Identifier(name)] type_:type_() [Comma]? { FGBinding { name, type_ } }

        rule method_specification() -> FGMethodSpecification<'a>
            = [Identifier(name)] [LeftParenthesis] parameters:binding_type()* [RightParenthesis] return_type:type_() { FGMethodSpecification { name, parameters, return_type } }

        rule type_() -> FGType<'a>
            = [Int] { FGType::Int }
            / [Identifier(name)] { FGType::Struct(name) }

        rule expression() -> FGExpression<'a> = precedence!{
            lhs:(@) [Plus] rhs:@ { FGExpression::BinOp { lhs: Box::new(lhs), operator: Operator::Add, rhs: Box::new(rhs) } }
            --
            lhs:(@) [Star] rhs:@ { FGExpression::BinOp { lhs: Box::new(lhs), operator: Operator::Mul, rhs: Box::new(rhs) } }
            --
            expression:(@) [Dot] [LeftParenthesis] assert:type_() [RightParenthesis] { FGExpression::TypeAssertion { expression: Box::new(expression), assert } }
            expression:(@) [Dot] [Identifier(method)] [LeftParenthesis] parameter_expressions:(expression() ** [Comma]) [RightParenthesis] {
                FGExpression::MethodCall { expression: Box::new(expression), method, parameter_expressions }
            }
            expression:(@) [Dot] [Identifier(field)] { FGExpression::Select { expression: Box::new(expression), field } }
            --
            [Identifier(name)] [LeftCurlyBrace] field_expressions:(expression() ** [Comma]) [RightCurlyBrace] { FGExpression::StructLiteral { name, field_expressions } }
            [Identifier(name)] { FGExpression::Variable { name } }
            [Number(value)] { FGExpression::Number { value } }
            [LeftParenthesis] expression:expression() [RightParenthesis] { expression }
        }
    }
);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FGProgram<'a> {
    pub declarations: Vec<FGDeclaration<'a>>,
    pub expression: Box<FGExpression<'a>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FGDeclaration<'a> {
    Type {
        name: &'a str,
        literal: FGTypeLiteral<'a>,
    },
    Method(FGMethodDeclaration<'a>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FGMethodDeclaration<'a> {
    pub receiver: FGBinding<'a, &'a str>,
    pub specification: FGMethodSpecification<'a>,
    pub body: FGExpression<'a>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FGTypeLiteral<'a> {
    Struct {
        fields: Vec<FGBinding<'a, FGType<'a>>>
    },
    Interface {
        methods: Vec<FGMethodSpecification<'a>>
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FGBinding<'a, T> {
    pub name: &'a str,
    pub type_: T,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FGExpression<'a> {
    Variable {
        name: &'a str
    },
    MethodCall {
        expression: Box<FGExpression<'a>>,
        method: &'a str,
        parameter_expressions: Vec<FGExpression<'a>>,
    },
    StructLiteral {
        name: &'a str,
        field_expressions: Vec<FGExpression<'a>>,
    },
    Select {
        expression: Box<FGExpression<'a>>,
        field: &'a str,
    },
    TypeAssertion {
        expression: Box<FGExpression<'a>>,
        assert: FGType<'a>,
    },
    Number {
        value: i64
    },
    BinOp {
        lhs: Box<FGExpression<'a>>,
        operator: Operator,
        rhs: Box<FGExpression<'a>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FGMethodSpecification<'a> {
    pub name: &'a str,
    pub parameters: Vec<FGBinding<'a, FGType<'a>>>,
    pub return_type: FGType<'a>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator {
    Add,
    Mul,
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum FGType<'a> {
    Int,
    Struct(&'a str),
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum TypeInfo<'a> {
    Struct(&'a Vec<FGBinding<'a, FGType<'a>>>, HashMap<&'a str, &'a FGMethodDeclaration<'a>>),
    Interface(&'a Vec<FGMethodSpecification<'a>>),
}

impl<'a> TypeInfo<'a> {
    pub fn method_spec(&self, method_name: &'a str) -> Option<&'a FGMethodSpecification<'a>> {
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

#[derive(Clone, Debug)]
pub enum FGValue<'a> {
    Int(i64),
    Struct(&'a str, Vec<FGValue<'a>>),
}