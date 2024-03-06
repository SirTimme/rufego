use Token;
use common::*;

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