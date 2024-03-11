use std::collections::HashMap;
use parser::{Expression, GenericBinding, GenericType, Operator, RufegoError};
use type_checker::{generate_substitution, is_subtype_of, substitute_struct_fields, substitute_type_parameter, SubstitutionMap, TypeInfo, TypeInfos};

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

fn substitute_expression<'a, 'b>(expression: &'a Expression<'b>, substitution: &'a SubstitutionMap<'b>) -> Result<Expression<'b>, RufegoError> {
    match expression {
        Expression::Variable { name } => {
            Ok(Expression::Variable { name })
        }
        Expression::MethodCall { expression, method, instantiation, parameter_expressions } => {
            let substituted_expression = substitute_expression(expression, substitution)?;
            let mut substituted_parameter_expressions = Vec::new();

            for parameter_expression in parameter_expressions {
                let substituted_parameter = substitute_expression(parameter_expression, substitution)?;
                substituted_parameter_expressions.push(substituted_parameter);
            }

            let mut substituted_instantiation = Vec::new();

            for instantiated_type in instantiation {
                let substituted_instantiation_type = substitute_type_parameter(instantiated_type, substitution);
                substituted_instantiation.push(substituted_instantiation_type)
            }

            Ok(Expression::MethodCall {
                expression: Box::new(substituted_expression),
                method,
                instantiation: substituted_instantiation,
                parameter_expressions: substituted_parameter_expressions,
            })
        }
        Expression::StructLiteral { name, instantiation, field_expressions } => {
            let mut substituted_field_expressions = Vec::new();

            for field_expression in field_expressions {
                let substituted_field = substitute_expression(field_expression, substitution)?;
                substituted_field_expressions.push(substituted_field)
            }

            let mut substituted_instantiation = Vec::new();

            for instantiated_type in instantiation {
                let substituted_instantiation_type = substitute_type_parameter(instantiated_type, substitution);
                substituted_instantiation.push(substituted_instantiation_type)
            }

            Ok(Expression::StructLiteral { name, instantiation: substituted_instantiation, field_expressions: substituted_field_expressions })
        }
        Expression::Select { expression, field } => {
            let substituted_expression = substitute_expression(expression, substitution)?;

            Ok(Expression::Select { expression: Box::new(substituted_expression), field })
        }
        Expression::TypeAssertion { expression, assert } => {
            let substituted_expression = substitute_expression(expression, substitution)?;
            let substituted_assert = substitute_type_parameter(assert, substitution);

            Ok(Expression::TypeAssertion { expression: Box::new(substituted_expression), assert: substituted_assert })
        }
        Expression::Number { .. } => Ok(expression.clone()),
        Expression::BinOp { lhs, operator, rhs } => {
            let lhs_substituted = substitute_expression(lhs, substitution)?;
            let rhs_substituted = substitute_expression(rhs, substitution)?;

            Ok(Expression::BinOp { lhs: Box::new(lhs_substituted), operator: *operator, rhs: Box::new(rhs_substituted) })
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

pub fn concat_substitutions<'a, 'b>(phi: &'a SubstitutionMap<'b>, psi: &'a SubstitutionMap<'b>) -> SubstitutionMap<'b> {
    let mut theta = SubstitutionMap::new();

    for (key, value) in phi {
        theta.insert(key, value.clone());
    }

    for (key, value) in psi {
        theta.insert(key, value.clone());
    }

    theta
}

pub(crate) fn body_of<'a, 'b>(
    receiver_name: &'b str,
    receiver_instantiation: &'a Vec<GenericType<'b>>,
    method_name: &'a str,
    method_instantiation: &'a Vec<GenericType<'b>>,
    type_infos: &'a TypeInfos<'b>,
) -> Result<(Vec<GenericBinding<'b>>, Expression<'b>), RufegoError> {
    match type_infos.get(receiver_name).unwrap() {
        TypeInfo::Struct { bound, methods, .. } => {
            let method_declaration = methods.get(method_name).unwrap();

            let struct_substitution = generate_substitution(bound, receiver_instantiation)?;
            let method_substitution = generate_substitution(&method_declaration.specification.bound, method_instantiation)?;

            let theta = concat_substitutions(&struct_substitution, &method_substitution);

            let substituted_expression = substitute_expression(&method_declaration.body, &theta)?;

            let mut parameters = Vec::new();

            parameters.push(GenericBinding { name: method_declaration.receiver.name, type_: GenericType::NamedType(receiver_name, receiver_instantiation.clone()) });

            for parameter in &method_declaration.specification.parameters {
                parameters.push(parameter.clone());
            }

            Ok((parameters, substituted_expression))
        }
        TypeInfo::Interface { .. } => {
            Err(RufegoError { message: format!("Tried to read body of interface method with type '{receiver_name}' ") })
        }
    }
}