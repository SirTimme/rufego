use std::collections::HashMap;
use parser::{FGExpression, FGType, FGValue, Operator, RufegoError, TypeInfo};
use type_checker::{is_subtype_of};

pub(crate) fn evaluate<'a>(expression: &FGExpression<'a>, context: &HashMap<&'a str, FGValue<'a>>, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<FGValue<'a>, RufegoError> {
    match expression {
        FGExpression::Variable { name } => {
            Ok(context.get(name).expect("Variable should exist in this context").clone())
        }
        FGExpression::MethodCall { expression, method, parameter_expressions } => {
            let value = evaluate(expression, context, types)?;

            match value {
                FGValue::Int(_) => {
                    Err(RufegoError { message: String::from("ERROR: Can't call a method on an integer value") })
                }
                FGValue::Struct(name, values) => {
                    let type_info = types.get(name).expect("Type name should exist");

                    match type_info {
                        TypeInfo::Struct(_, methods) => {
                            let method_declaration = methods.get(method).expect("Type should implement this method");

                            let mut local_context = HashMap::new();

                            local_context.insert(method_declaration.receiver.name, FGValue::Struct(method_declaration.receiver.type_, values));

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
                            Err(RufegoError { message: String::from("ERROR: Interface cant be called inside a methods body") })
                        }
                    }
                }
            }
        }
        FGExpression::StructLiteral { name, field_expressions } => {
            let mut values = Vec::new();

            for expression in field_expressions {
                let value = evaluate(expression, context, types)?;
                values.push(value);
            }

            Ok(FGValue::Struct(name, values))
        }
        FGExpression::Select { expression, field } => {
            let value = evaluate(expression, context, types)?;

            match value {
                FGValue::Int(_) => {
                    Err(RufegoError { message: String::from("An integer value doesn't have fields") })
                }
                FGValue::Struct(name, struct_values) => {
                    let type_info = types.get(name).expect("Value can only be a declared struct");

                    match type_info {
                        TypeInfo::Struct(fields, _) => {
                            let field_index = fields.iter().position(|binding| &binding.name == field).expect("Field name should exists");

                            Ok(struct_values.get(field_index).expect("Field should exists").clone())
                        }
                        TypeInfo::Interface(_) => {
                            Err(RufegoError { message: String::from("Cant instantiate an interface literal") })
                        }
                    }
                }
            }
        }
        FGExpression::TypeAssertion { expression, assert } => {
            let value = evaluate(expression, context, types)?;

            let value_type = match value {
                FGValue::Int(_) => {
                    FGType::Int
                }
                FGValue::Struct(name, _) => {
                    FGType::Struct(name)
                }
            };

            match is_subtype_of(&value_type, assert, types) {
                Ok(_) => {}
                Err(type_error) => {
                    return Err(RufegoError { message: format!("ERROR: Runtime-Check of type assertion failed with following error {:?}", type_error) });
                }
            }

            Ok(value)
        }
        FGExpression::Number { value } => {
            Ok(FGValue::Int(*value))
        }
        FGExpression::BinOp { lhs, operator, rhs } => {
            let lhs_value = evaluate(lhs, context, types)?;
            let rhs_value = evaluate(rhs, context, types)?;

            match (lhs_value, rhs_value) {
                (FGValue::Int(lhs), FGValue::Int(rhs)) => {
                    match operator {
                        Operator::Add => Ok(FGValue::Int(lhs + rhs)),
                        Operator::Mul => Ok(FGValue::Int(lhs * rhs)),
                    }
                }
                _ => {
                    Err(RufegoError { message: String::from("ERROR: LHS or RHS of a binary operation doesnt have a integer type") })
                }
            }
        }
    }
}