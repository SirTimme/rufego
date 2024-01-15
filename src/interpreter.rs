use std::collections::HashMap;
use std::process::exit;
use parser::{Expression, Operator};
use type_checker::{is_subtype_of, Type, TypeInfo};

#[derive(Clone, Debug)]
pub(crate) enum Value<'a> {
    Int(i64),
    Struct(&'a str, Vec<Value<'a>>),
}

pub(crate) fn evaluate<'a>(expression: &Expression<'a>, context: &HashMap<&'a str, Value<'a>>, types: &HashMap<&'a str, TypeInfo<'a>>) -> Value<'a> {
    match expression {
        Expression::Variable { name } => {
            context.get(name).expect("Variable should exist in this context").clone()
        }
        Expression::MethodCall { expression, method, parameter_expressions } => {
            let value = evaluate(expression, context, types);

            match value {
                Value::Int(_) => {
                    eprintln!("Can't call a method on an integer value");
                    exit(1);
                }
                Value::Struct(name, _) => {
                    let type_info = types.get(name).expect("Type name should exist");

                    match type_info {
                        TypeInfo::Struct(_, _) => {}
                        TypeInfo::Interface(_) => {}
                    }
                }
            }

            todo!()
        }
        Expression::StructLiteral { name, field_expressions } => {
            let mut values = Vec::new();

            for expression in field_expressions {
                let value = evaluate(expression, context, types);
                values.push(value);
            }

            Value::Struct(name, values)
        }
        Expression::Select { expression, field } => {
            let value = evaluate(expression, context, types);

            match value {
                Value::Int(_) => {
                    eprintln!("An integer value doesn't have fields");
                    exit(1);
                }
                Value::Struct(name, struct_values) => {
                    let type_info = types.get(name).expect("Value can only be a declared struct");

                    if let TypeInfo::Struct(fields, _) = type_info {
                        let field_index = fields.iter().position(|binding| &binding.name == field).expect("Field name should exists");

                        struct_values.get(field_index).expect("Field should exists").clone()
                    } else {
                        eprintln!("Cant instantiate an interface literal");
                        exit(1);
                    }
                }
            }
        }
        Expression::TypeAssertion { expression, assert } => {
            let value = evaluate(expression, context, types);

            let value_type = match value {
                Value::Int(_) => {
                    Type::Int
                }
                Value::Struct(name, _) => {
                    Type::Struct(name)
                }
            };

            if !is_subtype_of(&value_type, assert, types) {
                eprintln!("ERROR: Runtime-Check of assertion failed {:?} is not a subtype of {:?}", value_type, assert);
                exit(1);
            }

            value
        }
        Expression::Number { value } => {
            Value::Int(*value)
        }
        Expression::BinOp { lhs, operator, rhs } => {
            let lhs_value = evaluate(lhs, context, types);
            let rhs_value = evaluate(rhs, context, types);

            match (lhs_value, rhs_value) {
                (Value::Int(lhs), Value::Int(rhs)) => {
                    match operator {
                        Operator::Add => Value::Int(lhs + rhs),
                        Operator::Mul => Value::Int(lhs * rhs),
                    }
                }
                _ => {
                    eprintln!("ERROR: LHS or RHS of a binary operation doesnt have a integer type");
                    exit(1);
                }
            }
        }
    }
}