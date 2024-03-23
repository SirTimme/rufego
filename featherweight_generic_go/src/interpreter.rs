use std::collections::HashMap;
use common::parser::{Expression, GenericType, Operator, RufegoError};
use common::{body_of, generate_substitution, is_subtype_of, substitute_struct_fields, TypeInfo, TypeInfos};

#[derive(Clone, Debug)]
pub(crate) enum Value<'a> {
    Int(i64),
    Struct {
        name: &'a str,
        instantiation: Vec<GenericType<'a>>,
        values: Vec<Value<'a>>,
    },
}

pub(crate) fn evaluate<'a, 'b>(expression: &'a Expression<'b>, variables: &'a HashMap<&'a str, Value<'b>>, type_infos: &'a TypeInfos<'b>) -> Result<Value<'b>, RufegoError> {
    match expression {
        Expression::Variable { name } => {
            Ok(variables.get(name).unwrap().clone())
        }
        Expression::MethodCall { expression, method, instantiation: method_instantiation, parameter_expressions } => {
            let value = evaluate(expression, variables, type_infos)?;

            match value {
                Value::Int(_) => Err(RufegoError { message: format!("Tried to call method '{method}' on a number value") }),
                Value::Struct { name, instantiation, values } => {
                    let (mut parameters, substituted_expression) = body_of(name, &instantiation, method, method_instantiation, type_infos)?;

                    // substitute receiver and parameter with evaluated values
                    let mut local_variables = HashMap::new();

                    // receiver binding is stored at index 0 so remove it
                    let receiver_binding = parameters.remove(0);

                    // insert receiver to local variables
                    local_variables.insert(receiver_binding.name, Value::Struct { name, instantiation: instantiation.clone(), values: values.clone() });

                    for (index, parameter_binding) in parameters.iter().enumerate() {
                        let expression = parameter_expressions.get(index).unwrap();
                        let expression_value = evaluate(expression, variables, type_infos)?;

                        local_variables.insert(parameter_binding.name, expression_value);
                    }

                    Ok(evaluate(&substituted_expression, &local_variables, type_infos)?)
                }
            }
        }
        Expression::StructLiteral { name, instantiation, field_expressions } => {
            let mut field_values = Vec::new();

            for expression in field_expressions {
                let field_value = evaluate(expression, variables, type_infos)?;
                field_values.push(field_value);
            }

            Ok(Value::Struct {
                name,
                instantiation: instantiation.clone(),
                values: field_values,
            })
        }
        Expression::Select { expression, field } => {
            let value = evaluate(expression, variables, type_infos)?;

            match value {
                Value::Int(_) => {
                    Err(RufegoError { message: String::from("Select expression evaluated to a number type") })
                }
                Value::Struct { name, instantiation, values } => {
                    let type_info = match type_infos.get(name) {
                        None => {
                            return Err(RufegoError { message: format!("Literal '{name}' is not declared") });
                        }
                        Some(type_info) => {
                            type_info
                        }
                    };

                    match type_info {
                        TypeInfo::Struct { bound, fields, .. } => {
                            // generate substitution map for struct fields
                            let substitution = generate_substitution(bound, &instantiation)?;

                            // substitute fields with instantiation if possible
                            let substituted_struct_fields = substitute_struct_fields(&substitution, fields);

                            for (index, field_binding) in substituted_struct_fields.iter().enumerate() {
                                if &field_binding.name == field {
                                    let selected_value = values.get(index).unwrap();
                                    return Ok(selected_value.clone());
                                }
                            }

                            Err(RufegoError { message: format!("Struct type '{name}' does not have a field named '{field}'") })
                        }
                        TypeInfo::Interface { .. } => {
                            Err(RufegoError { message: format!("Tried to select on type '{name}' which is an interface") })
                        }
                    }
                }
            }
        }
        Expression::TypeAssertion { expression, assert } => {
            let value = evaluate(expression, variables, type_infos)?;

            match is_subtype_of(&type_of(&value), assert, &HashMap::new(), type_infos) {
                Ok(_) => {}
                Err(error) => {
                    return Err(RufegoError { message: format!("Runtime assertion for type '{}' failed:\n{}", type_of(&value).name(), error.message) });
                }
            }

            Ok(value)
        }
        Expression::Number { value } => {
            Ok(Value::Int(*value))
        }
        Expression::BinOp { lhs, operator, rhs } => {
            let lhs_value = evaluate(lhs, variables, type_infos)?;
            let rhs_value = evaluate(rhs, variables, type_infos)?;

            match (&lhs_value, &rhs_value) {
                (Value::Int(lhs), Value::Int(rhs)) => {
                    match operator {
                        Operator::Add => Ok(Value::Int(lhs + rhs)),
                        Operator::Mul => Ok(Value::Int(lhs * rhs)),
                    }
                }
                _ => {
                    let error_message = format!("Runtime evaluation of a binary operation failed: LHS was type '{}' RHS was type {}",
                                                type_of(&lhs_value).name(),
                                                type_of(&rhs_value).name()
                    );
                    Err(RufegoError { message: error_message })
                }
            }
        }
    }
}


fn type_of<'a>(value: &'a Value) -> GenericType<'a> {
    match value {
        Value::Int(_) => {
            GenericType::NumberType
        }
        Value::Struct { name, instantiation, .. } => {
            GenericType::NamedType(name, instantiation.clone())
        }
    }
}

