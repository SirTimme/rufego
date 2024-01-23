use std::collections::HashMap;
use parser::{Expression, Operator};
use type_checker::{is_subtype_of, Type, TypeInfo};

#[derive(Clone, Debug)]
pub(crate) enum Value<'a> {
    Int(i64),
    Struct(&'a str, Vec<Value<'a>>),
}

#[derive(Debug)]
pub(crate) struct EvalError {
    pub(crate) message: String,
}

pub(crate) fn evaluate<'a>(expression: &Expression<'a>, context: &HashMap<&'a str, Value<'a>>, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<Value<'a>, EvalError> {
    match expression {
        Expression::Variable { name } => {
            Ok(context.get(name).expect("Variable should exist in this context").clone())
        }
        Expression::MethodCall { expression, method, parameter_expressions } => {
            let value = evaluate(expression, context, types)?;

            match value {
                Value::Int(_) => {
                    Err(EvalError { message: String::from("ERROR: Can't call a method on an integer value") })
                }
                Value::Struct(name, values) => {
                    let type_info = types.get(name).expect("Type name should exist");

                    match type_info {
                        TypeInfo::Struct(_, methods) => {
                            let method_declaration = methods.get(method).expect("Type should implement this method");

                            // check for recursive method call
                            if is_recursive_methodcall(&method_declaration.body) {
                                return Err(EvalError { message: format!("ERROR: Body of method {:?} contains a recursive method call", method_declaration.specification.name) });
                            }

                            let mut local_context = HashMap::new();

                            local_context.insert(method_declaration.receiver.name, Value::Struct(method_declaration.receiver.type_, values));

                            for (index, expression) in parameter_expressions.iter().enumerate() {
                                if let Some(parameter) = method_declaration.specification.parameters.get(index) {
                                    // evaluate type of the supplied parameter expression
                                    let expression_type = evaluate(expression, context, types)?;

                                    local_context.insert(parameter.name, expression_type);
                                }
                            }

                            Ok(evaluate(&method_declaration.body, &local_context, types)?)
                        }
                        TypeInfo::Interface(_) => {
                            Err(EvalError { message: String::from("ERROR: Interface cant be called inside a methods body") })
                        }
                    }
                }
            }
        }
        Expression::StructLiteral { name, field_expressions } => {
            let mut values = Vec::new();

            for expression in field_expressions {
                let value = evaluate(expression, context, types)?;
                values.push(value);
            }

            Ok(Value::Struct(name, values))
        }
        Expression::Select { expression, field } => {
            let value = evaluate(expression, context, types)?;

            match value {
                Value::Int(_) => {
                    Err(EvalError { message: String::from("An integer value doesn't have fields") })
                }
                Value::Struct(name, struct_values) => {
                    let type_info = types.get(name).expect("Value can only be a declared struct");

                    if let TypeInfo::Struct(fields, _) = type_info {
                        let field_index = fields.iter().position(|binding| &binding.name == field).expect("Field name should exists");

                        Ok(struct_values.get(field_index).expect("Field should exists").clone())
                    } else {
                        Err(EvalError { message: String::from("Cant instantiate an interface literal") })
                    }
                }
            }
        }
        Expression::TypeAssertion { expression, assert } => {
            let value = evaluate(expression, context, types)?;

            let value_type = match value {
                Value::Int(_) => {
                    Type::Int
                }
                Value::Struct(name, _) => {
                    Type::Struct(name)
                }
            };

            match is_subtype_of(&value_type, assert, types) {
                Ok(_) => {}
                Err(type_error) => {
                    return Err(EvalError { message: format!("ERROR: Runtime-Check of type assertion failed with following error {:?}", type_error) });
                }
            }

            Ok(value)
        }
        Expression::Number { value } => {
            Ok(Value::Int(*value))
        }
        Expression::BinOp { lhs, operator, rhs } => {
            let lhs_value = evaluate(lhs, context, types)?;
            let rhs_value = evaluate(rhs, context, types)?;

            match (lhs_value, rhs_value) {
                (Value::Int(lhs), Value::Int(rhs)) => {
                    match operator {
                        Operator::Add => Ok(Value::Int(lhs + rhs)),
                        Operator::Mul => Ok(Value::Int(lhs * rhs)),
                    }
                }
                _ => {
                    Err(EvalError { message: String::from("ERROR: LHS or RHS of a binary operation doesnt have a integer type") })
                }
            }
        }
    }
}

fn is_recursive_methodcall(expression: &Expression) -> bool {
    eprintln!("{:#?}", expression);
    false
}