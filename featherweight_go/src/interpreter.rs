use std::collections::HashMap;
use parser::{Expression, Type, Operator, RufegoError, TypeInfo, Binding};
use type_checker::{is_subtype_of, TypeInfos};


#[derive(Clone, Debug)]
pub enum Value<'a> {
    Int(i64),
    Struct(&'a str, Vec<Value<'a>>),
}

pub(crate) fn evaluate_expression<'a>(expression: &Expression<'a>, values: &HashMap<&'a str, Value<'a>>, types: &'a TypeInfos) -> Result<Value<'a>, RufegoError> {
    match expression {
        Expression::Variable { name } => {
            Ok(values.get(name).unwrap().clone())
        }
        Expression::MethodCall { expression, method, parameter_expressions } => {
            let value = evaluate_expression(expression, values, types)?;

            match value {
                Value::Int(_) => {
                    Err(RufegoError { message: String::from("ERROR: Can't call a method on an integer value") })
                }
                Value::Struct(name, struct_values) => {
                    let (mut parameters, expression) = body_of(name, method, types)?;
                    let mut local_values = HashMap::new();

                    let receiver_binding = parameters.remove(0);

                    local_values.insert(receiver_binding.name, Value::Struct(name, struct_values));

                    for (index, parameter_binding) in parameters.iter().enumerate() {
                        let parameter_expression = parameter_expressions.get(index).unwrap();
                        let parameter_value = evaluate_expression(parameter_expression, values, types)?;

                        local_values.insert(parameter_binding.name, parameter_value);
                    }

                    Ok(evaluate_expression(&expression, &local_values, types)?)
                }
            }
        }
        Expression::StructLiteral { name, field_expressions } => {
            let mut struct_values = Vec::new();

            for expression in field_expressions {
                let value = evaluate_expression(expression, values, types)?;
                struct_values.push(value);
            }

            Ok(Value::Struct(name, struct_values))
        }
        Expression::Select { expression, field } => {
            let value = evaluate_expression(expression, values, types)?;

            match value {
                Value::Int(_) => {
                    Err(RufegoError { message: String::from("An integer value doesn't have fields") })
                }
                Value::Struct(name, struct_values) => {
                    let type_info = types.get(name).expect("Value can only be a declared struct");

                    match type_info {
                        TypeInfo::Struct(fields, _) => {
                            let field_index = fields.iter().position(|binding| &binding.name == field).expect("Field name should exists");

                            Ok(struct_values.get(field_index).unwrap().clone())
                        }
                        TypeInfo::Interface(_) => {
                            Err(RufegoError { message: String::from("Cant instantiate an interface literal") })
                        }
                    }
                }
            }
        }
        Expression::TypeAssertion { expression, assert } => {
            let value = evaluate_expression(expression, values, types)?;

            match is_subtype_of(&type_of(&value), assert, types) {
                Ok(_) => {}
                Err(type_error) => {
                    return Err(RufegoError { message: format!("ERROR: Runtime-Check of type assertion failed with following error {:?}", type_error) });
                }
            }

            Ok(value)
        }
        Expression::Number { value } => {
            Ok(Value::Int(*value))
        }
        Expression::BinOp { lhs, operator, rhs } => {
            let lhs_value = evaluate_expression(lhs, values, types)?;
            let rhs_value = evaluate_expression(rhs, values, types)?;

            match (lhs_value, rhs_value) {
                (Value::Int(lhs), Value::Int(rhs)) => {
                    match operator {
                        Operator::Add => Ok(Value::Int(lhs + rhs)),
                        Operator::Mul => Ok(Value::Int(lhs * rhs)),
                    }
                }
                _ => {
                    Err(RufegoError { message: String::from("ERROR: LHS or RHS of a binary operation doesnt have a integer type") })
                }
            }
        }
    }
}

fn type_of<'a>(value: &'a Value) -> Type<'a> {
    match value {
        Value::Int(_) => {
            Type::Int
        }
        Value::Struct(name, _) => {
            Type::Struct(name)
        }
    }
}

pub(crate) fn body_of<'a>(
    receiver_name: &'a str,
    method_name: &'a str,
    type_infos: &'a TypeInfos,
) -> Result<(Vec<Binding<'a, Type<'a>>>, Expression<'a>), RufegoError> {
    match type_infos.get(receiver_name).unwrap() {
        TypeInfo::Struct(_, methods) => {
            let method_declaration = methods.get(method_name).unwrap();
            let mut parameters = Vec::new();

            parameters.push(Binding { name: method_declaration.receiver.name, type_: Type::Struct(receiver_name) });

            for parameter in &method_declaration.specification.parameters {
                parameters.push(parameter.clone());
            }

            Ok((parameters, method_declaration.body.clone()))
        }
        TypeInfo::Interface(_) => {
            Err(RufegoError { message: format!("Tried to read body of interface method with type '{receiver_name}' ") })
        }
    }
}