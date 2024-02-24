use std::collections::HashMap;
use parser::{Expression, GenericType, Operator};
use type_checker::{generate_substitution, is_subtype_of, substitute_struct_fields, SubstitutionMap, TypeInfo, TypeInfos};

#[derive(Clone, Debug)]
pub(crate) enum Value<'a> {
    Int(i64),
    Struct {
        name: &'a str,
        instantiation: Vec<GenericType<'a>>,
        values: Vec<Value<'a>>,
    },
}

#[derive(Debug)]
pub(crate) struct EvalError {
    pub(crate) message: String,
}

pub(crate) fn evaluate<'a, 'b>(expression: &'a Expression<'b>, variables: &'a HashMap<&'a str, Value<'b>>, type_infos: &'a TypeInfos<'b>) -> Result<Value<'b>, EvalError> {
    match expression {
        Expression::Variable { name } => {
            Ok(variables.get(name).unwrap().clone())
        }
        Expression::MethodCall { expression, method, instantiation: method_instantiation, parameter_expressions } => {
            let value = evaluate(expression, variables, type_infos)?;

            match value {
                Value::Int(_) => Err(EvalError { message: String::from("ERROR: Can't call a method on an integer value") }),
                Value::Struct { name, instantiation, values } => {
                    let type_info = type_infos.get(name).unwrap();

                    match type_info {
                        TypeInfo::Struct { bound, fields, methods } => {
                            let method_declaration = methods.get(method).unwrap();
                            let method_substitution = generate_substitution(&method_declaration.specification.bound, method_instantiation).unwrap();
                            
                            let mut struct_substitution = generate_substitution(bound, &instantiation).unwrap();
                            struct_substitution.extend(method_substitution);
                                                        
                            let substituted_expression = substitute_expression(&method_declaration.body, &struct_substitution)?;
                            
                            println!("Sub Expr {:#?}", substituted_expression);
                            
                            let mut local_variables = HashMap::new();
                            
                            // insert receiver to variables
                            local_variables.insert(method_declaration.receiver.name, Value::Struct { name, instantiation, values });

                            // insert method parameters in the local context
                            for (index, expression) in parameter_expressions.iter().enumerate() {
                                if let Some(parameter) = method_declaration.specification.parameters.get(index) {
                                    // evaluate type of the supplied parameter expression
                                    let expression_type = evaluate(expression, variables, type_infos)?;
                                    local_variables.insert(parameter.name, expression_type);
                                }
                            }
            
                            Ok(evaluate(&substituted_expression, &local_variables, type_infos)?)
                        }
                        TypeInfo::Interface { .. } => Err(EvalError { message: String::from("ERROR: Interface cant be called inside a methods body") }),
                    }
                }
            }
        }
        Expression::StructLiteral { name, instantiation, field_expressions } => {
            let mut values = Vec::new();

            for expression in field_expressions {
                let value = evaluate(expression, variables, type_infos)?;
                values.push(value);
            }

            Ok(Value::Struct {
                name,
                instantiation: instantiation.clone(),
                values,
            })
        }
        Expression::Select { expression, field } => {
            let value = evaluate(expression, variables, type_infos)?;

            match value {
                Value::Int(_) => {
                    Err(EvalError { message: String::from("An integer value doesn't have fields") })
                }
                Value::Struct { name, instantiation, values } => {
                    match type_infos.get(name).unwrap() {
                        TypeInfo::Struct { bound, fields, methods } => {
                            // generate substitution map for struct fields
                            let substitution = generate_substitution(bound, &instantiation).unwrap();

                            // substitute fields with type parameters with their instantiation
                            let substituted_struct_fields = substitute_struct_fields(&substitution, fields).unwrap();

                            let field_index = substituted_struct_fields.iter().position(|field_binding| &field_binding.name == field).unwrap();

                            Ok(values.get(field_index).unwrap().clone())
                        }
                        TypeInfo::Interface { .. } => {
                            Err(EvalError { message: String::from("Cant instantiate an interface literal") })
                        }
                    }
                }
            }
        }
        Expression::TypeAssertion { expression, assert } => {
            let value = evaluate(expression, variables, type_infos)?;

            let value_type = match &value {
                Value::Int(_) => GenericType::NumberType,
                Value::Struct { name, instantiation, .. } => GenericType::NamedType(name, instantiation.clone()),
            };

            let mut environment = HashMap::new();

            for (key, context_value) in variables {
                match context_value {
                    Value::Int(_) => {
                        environment.insert(*key, GenericType::NumberType);
                    }
                    Value::Struct { name, instantiation, values } => {
                        environment.insert(*key, GenericType::NamedType(name, instantiation.clone()));
                    }
                }
            }

            match is_subtype_of(&value_type, assert, &environment, type_infos) {
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
            let lhs_value = evaluate(lhs, variables, type_infos)?;
            let rhs_value = evaluate(rhs, variables, type_infos)?;

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

fn substitute_expression<'a, 'b>(expression: &'a Expression<'b>, substitution: &'a SubstitutionMap<'b>) -> Result<Expression<'b>, EvalError> {
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

            Ok(Expression::MethodCall { 
                expression: Box::new(substituted_expression), 
                method, 
                instantiation: instantiation.clone(), 
                parameter_expressions: substituted_parameter_expressions 
            })
        }
        Expression::StructLiteral { name, instantiation, field_expressions } => {
            let mut substituted_field_expressions = Vec::new();

            for field_expression in field_expressions {
                let substituted_field = substitute_expression(field_expression, substitution)?;
                substituted_field_expressions.push(substituted_field)
            }

            Ok(Expression::StructLiteral { name, instantiation: instantiation.clone(), field_expressions: substituted_field_expressions })
        }
        Expression::Select { expression, field } => {
            let substituted_expression = substitute_expression(expression, substitution)?;

            Ok(Expression::Select { expression: Box::new(substituted_expression), field })
        }
        Expression::TypeAssertion { expression, assert } => {
            let substituted_expression = substitute_expression(expression, substitution)?;
            let substituted_assert = match substitution.get(assert.name()) {
                None => assert,
                Some(entry) => entry
            };

            Ok(Expression::TypeAssertion { expression: Box::new(substituted_expression), assert: substituted_assert.clone() })
        }
        Expression::Number { .. } => Ok(expression.clone()),
        Expression::BinOp { lhs, operator, rhs } => {
            let lhs_substituted = substitute_expression(lhs, substitution)?;
            let rhs_substituted = substitute_expression(rhs, substitution)?;

            Ok(Expression::BinOp { lhs: Box::new(lhs_substituted), operator: *operator, rhs: Box::new(rhs_substituted) })
        }
    }
}