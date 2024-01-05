use Token;

peg::parser!(
    pub(crate) grammar language<'a>() for [Token<'a>] {
        use Token::*;

        pub(crate) rule program() -> Program<'a>
            = [Package] [Main] [Semicolon] declarations:declaration()* [Function] [Main] [LeftParenthesis] [RightParenthesis] [LeftCurlyBrace] [Underscore] [Equals] expression:expression() [RightCurlyBrace] {
                Program { declarations, expression: Box::new(expression) }
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
            = [Identifier(name)] [Identifier(type_)] [Comma]? {
                Binding { name, type_ }
            }

        rule method_body() -> MethodBody<'a>
            = [Identifier(name)] [LeftParenthesis] params:binding()* [RightParenthesis] [Identifier(return_type)] {
                MethodBody { name, params, return_type }
            }

        #[cache]
        rule expression() -> Expression<'a> = precedence!{
            expression:(@) [Dot] [LeftParenthesis] [Identifier(assertion)] [RightParenthesis] {
                Expression::TypeAssertion { expression: Box::new(expression), assertion }
            }
            expression:(@) [Dot] [Identifier(method)] [LeftParenthesis] parameters:(expression() ** [Comma]) [RightParenthesis] {
                Expression::MethodCall { expression: Box::new(expression), method, parameters }
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
            [LeftParenthesis] expression:expression() [RightParenthesis] {
                expression
            }
        }
    }
);

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Program<'a> {
    pub(crate) declarations: Vec<Declaration<'a>>,
    expression: Box<Expression<'a>>
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Declaration<'a> {
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
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum TypeLiteral<'a> {
    Struct {
        name: &'a str,
        fields: Vec<Binding<'a>>
    },
    Interface {
        name: &'a str,
        methods: Vec<MethodBody<'a>>
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Binding<'a> {
    pub(crate) name: &'a str,
    pub(crate) type_: &'a str,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Expression<'a> {
    Variable {
        name: &'a str
    },
    MethodCall {
        expression: Box<Expression<'a>>,
        method: &'a str,
        parameters: Vec<Expression<'a>>
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
        assertion: &'a str
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct MethodBody<'a> {
    pub(crate) name: &'a str,
    pub(crate) params: Vec<Binding<'a>>,
    pub(crate) return_type: &'a str,
}