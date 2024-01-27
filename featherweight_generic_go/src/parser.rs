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
            / [Int] { GenericType::NumberType }
            / [Identifier(name)] { GenericType::TypeParameter(name) }

        rule generic_receiver() -> GenericReceiver<'a>
            = [Identifier(receiver_var)] [Identifier(receiver_type)] [LeftParenthesis] generic_params:formal_type() [RightParenthesis] { GenericReceiver { name: receiver_var, type_: receiver_type, instantiation: generic_params } }

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
            expression:(@) [Dot] [LeftParenthesis] assert:generic_type() [RightParenthesis] {
                Expression::TypeAssertion { expression: Box::new(expression), assert }
            }
            expression:(@) [Dot] [Identifier(method)] [LeftParenthesis] bound:(generic_type() ** [Comma]) [RightParenthesis] [LeftParenthesis] parameter_expressions:(expression() ** [Comma]) [RightParenthesis] {
                Expression::MethodCall { expression: Box::new(expression), method, bound, parameter_expressions }
            }
            expression:(@) [Dot] [Identifier(field)] {
                Expression::Select { expression: Box::new(expression), field }
            }
            --
            [Identifier(name)] [LeftParenthesis] bound:(generic_type() ** [Comma]) [RightParenthesis] [LeftCurlyBrace] field_expressions:(expression() ** [Comma]) [RightCurlyBrace] {
                Expression::StructLiteral { name, bound, field_expressions }
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
    pub(crate) expression: Box<Expression<'a>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Declaration<'a> {
    Type {
        name: &'a str,
        bound: Vec<GenericBinding<'a>>,
        literal: TypeLiteral<'a>,
    },
    Method(MethodDeclaration<'a>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct MethodDeclaration<'a> {
    pub(crate) receiver: GenericReceiver<'a>,
    pub(crate) specification: MethodSpecification<'a>,
    pub(crate) body: Expression<'a>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum TypeLiteral<'a> {
    Struct {
        fields: Vec<GenericBinding<'a>>
    },
    Interface {
        methods: Vec<MethodSpecification<'a>>
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum GenericType<'a> {
    TypeParameter(&'a str),
    NamedType(&'a str, Vec<GenericType<'a>>),
    NumberType,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct GenericBinding<'a> {
    pub(crate) name: &'a str,
    pub(crate) type_: GenericType<'a>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct GenericReceiver<'a> {
    pub(crate) name: &'a str,
    pub(crate) type_: &'a str,
    pub(crate) instantiation: Vec<GenericBinding<'a>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Expression<'a> {
    Variable {
        name: &'a str
    },
    MethodCall {
        expression: Box<Expression<'a>>,
        method: &'a str,
        bound: Vec<GenericType<'a>>,
        parameter_expressions: Vec<Expression<'a>>,
    },
    StructLiteral {
        name: &'a str,
        bound: Vec<GenericType<'a>>,
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct MethodSpecification<'a> {
    pub(crate) name: &'a str,
    pub(crate) bound: Vec<GenericBinding<'a>>,
    pub(crate) parameters: Vec<GenericBinding<'a>>,
    pub(crate) return_type: GenericType<'a>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator {
    Add,
    Mul,
}