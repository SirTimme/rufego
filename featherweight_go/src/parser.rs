use Token;
use type_checker::Type as TypeEnum;

peg::parser!(
    pub(crate) grammar language<'a>() for [Token<'a>] {
        use Token::*;

        pub(crate) rule parse() -> Program<'a>
            = [Package] [Main] [Semicolon] declarations:declaration()* [Function] [Main] [LeftParenthesis] [RightParenthesis] [LeftCurlyBrace] [Underscore] [Equals] expression:expression() [RightCurlyBrace] {
                Program { declarations, expression: Box::new(expression) }
            }

        rule declaration() -> Declaration<'a>
            = [Function] [LeftParenthesis] receiver:binding_string() [RightParenthesis] [Identifier(name)] [LeftParenthesis] parameters:binding_type()* [RightParenthesis] return_type:type_() [LeftCurlyBrace] [Return] body:expression() [RightCurlyBrace] {
                Declaration::Method(MethodDeclaration { receiver, specification: MethodSpecification { name, parameters, return_type }, body })
            }
            / literal:type_literal() {
                Declaration::Type { literal }
            }

        rule type_literal() -> TypeLiteral<'a>
            = [Type] [Identifier(name)] [Struct] [LeftCurlyBrace] fields:binding_type()* [RightCurlyBrace] {
                TypeLiteral::Struct { name, fields }
            }
            / [Type] [Identifier(name)] [Interface] [LeftCurlyBrace] methods:method_body()* [RightCurlyBrace] {
                TypeLiteral::Interface { name, methods }
            }

        rule binding_string() -> Binding<'a, &'a str>
            = [Identifier(name)] [Identifier(type_)] [Comma]? {
                Binding { name, type_ }
            }

        rule binding_type() -> Binding<'a, TypeEnum<'a>>
            = [Identifier(name)] type_:type_() [Comma]? {
                Binding { name, type_ }
            }

        rule method_body() -> MethodSpecification<'a>
            = [Identifier(name)] [LeftParenthesis] parameters:binding_type()* [RightParenthesis] return_type:type_() {
                MethodSpecification { name, parameters, return_type }
            }

        rule type_() -> TypeEnum<'a>
            = [Int] { TypeEnum::Int }
            / [Identifier(name)] { TypeEnum::Struct(name) }

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
            expression:(@) [Dot] [LeftParenthesis] assert:type_() [RightParenthesis] {
                Expression::TypeAssertion { expression: Box::new(expression), assert }
            }
            expression:(@) [Dot] [Identifier(method)] [LeftParenthesis] parameter_expressions:(expression() ** [Comma]) [RightParenthesis] {
                Expression::MethodCall { expression: Box::new(expression), method, parameter_expressions }
            }
            expression:(@) [Dot] [Identifier(field)] {
                Expression::Select { expression: Box::new(expression), field }
            }
            --
            [Identifier(name)] [LeftCurlyBrace] field_expressions:(expression() ** [Comma]) [RightCurlyBrace] {
                Expression::StructLiteral { name, field_expressions }
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
pub(crate) struct Program<'a> {
    pub(crate) declarations: Vec<Declaration<'a>>,
    pub(crate) expression: Box<Expression<'a>>
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Declaration<'a> {
    Type {
        literal: TypeLiteral<'a>
    },
    Method(MethodDeclaration<'a>)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct MethodDeclaration<'a> {
    pub(crate) receiver: Binding<'a, &'a str>,
    pub(crate) specification: MethodSpecification<'a>,
    pub(crate) body: Expression<'a>
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum TypeLiteral<'a> {
    Struct {
        name: &'a str,
        fields: Vec<Binding<'a, TypeEnum<'a>>>
    },
    Interface {
        name: &'a str,
        methods: Vec<MethodSpecification<'a>>
    }
}

impl<'a> TypeLiteral<'a> {
    pub fn name(&self) -> &str {
        match self {
            TypeLiteral::Struct { name, .. } => name,
            TypeLiteral::Interface { name, .. } => name,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Binding<'a, T> {
    pub(crate) name: &'a str,
    pub(crate) type_: T,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Expression<'a> {
    Variable {
        name: &'a str
    },
    MethodCall {
        expression: Box<Expression<'a>>,
        method: &'a str,
        parameter_expressions: Vec<Expression<'a>>
    },
    StructLiteral {
        name: &'a str,
        field_expressions: Vec<Expression<'a>>
    },
    Select {
        expression: Box<Expression<'a>>,
        field: &'a str
    },
    TypeAssertion {
        expression: Box<Expression<'a>>,
        assert: TypeEnum<'a>
    },
    Number {
        value: i64
    },
    BinOp {
        lhs: Box<Expression<'a>>,
        operator: Operator,
        rhs: Box<Expression<'a>>
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct MethodSpecification<'a> {
    pub(crate) name: &'a str,
    pub(crate) parameters: Vec<Binding<'a, TypeEnum<'a>>>,
    pub(crate) return_type: TypeEnum<'a>
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator {
    Add,
    Mul,
}