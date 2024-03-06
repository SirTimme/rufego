use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FGProgram<'a> {
    pub declarations: Vec<FGDeclaration<'a>>,
    pub expression: Box<FGExpression<'a>>
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FGDeclaration<'a> {
    Type {
        name: &'a str,
        literal: FGTypeLiteral<'a>
    },
    Method(FGMethodDeclaration<'a>)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FGMethodDeclaration<'a> {
    pub receiver: FGBinding<'a, &'a str>,
    pub specification: FGMethodSpecification<'a>,
    pub body: FGExpression<'a>
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FGTypeLiteral<'a> {
    Struct {
        fields: Vec<FGBinding<'a, FGType<'a>>>
    },
    Interface {
        methods: Vec<FGMethodSpecification<'a>>
    }
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
        parameter_expressions: Vec<FGExpression<'a>>
    },
    StructLiteral {
        name: &'a str,
        field_expressions: Vec<FGExpression<'a>>
    },
    Select {
        expression: Box<FGExpression<'a>>,
        field: &'a str
    },
    TypeAssertion {
        expression: Box<FGExpression<'a>>,
        assert: FGType<'a>
    },
    Number {
        value: i64
    },
    BinOp {
        lhs: Box<FGExpression<'a>>,
        operator: Operator,
        rhs: Box<FGExpression<'a>>
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FGMethodSpecification<'a> {
    pub name: &'a str,
    pub parameters: Vec<FGBinding<'a, FGType<'a>>>,
    pub return_type: FGType<'a>
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

impl<'a> From<&'a str> for FGType<'a> {
    fn from(value: &'a str) -> Self {
        if value == "int" {
            Self::Int
        } else {
            Self::Struct(value)
        }
    }
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