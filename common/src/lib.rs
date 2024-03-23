use std::collections::HashMap;
use crate::parser::{Declaration, Expression, GenericBinding, GenericType, MethodDeclaration, MethodSpecification, Program, RufegoError, TypeLiteral};

pub mod token;
pub mod parser;

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum TypeInfo<'a> {
    Struct {
        bound: &'a Vec<GenericBinding<'a>>,
        fields: &'a Vec<GenericBinding<'a>>,
        methods: HashMap<&'a str, &'a MethodDeclaration<'a>>,
    },
    Interface {
        bound: &'a Vec<GenericBinding<'a>>,
        methods: &'a Vec<MethodSpecification<'a>>,
    },
}

// Type name -> Type Info
pub type TypeInfos<'a> = HashMap<&'a str, TypeInfo<'a>>;

// Type parameter -> Bound
pub type TypeEnvironment<'a> = HashMap<&'a str, GenericType<'a>>;

// Variables -> Type
pub type VariableEnvironment<'a> = HashMap<&'a str, GenericType<'a>>;

// Type parameter -> Actual type instantiation
pub type SubstitutionMap<'a> = HashMap<&'a str, GenericType<'a>>;

pub fn generate_substitution<'a, 'b>(type_formals: &'a Vec<GenericBinding<'b>>, instantiation: &'a Vec<GenericType<'b>>) -> Result<SubstitutionMap<'b>, RufegoError> {
    let mut generated_substitution = HashMap::new();

    // correct amount of type parameters supplied?
    if type_formals.len() != instantiation.len() {
        return Err(RufegoError { message: format!("Type formals consists of '{}' parameters but '{}' parameters were provided", type_formals.len(), instantiation.len()) });
    }

    // substitute type formals with type actuals and check bound
    for (index, formal_type) in type_formals.iter().enumerate() {
        let actual_type = instantiation.get(index).unwrap();
        generated_substitution.insert(formal_type.name, actual_type.clone());
    }

    Ok(generated_substitution)
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

pub fn body_of<'a, 'b>(
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

pub fn substitute_type_parameter<'a, 'b>(type_: &'a GenericType<'b>, substitution: &'a SubstitutionMap<'b>) -> GenericType<'b> {
    match type_ {
        GenericType::TypeParameter(type_name) => {
            match substitution.get(type_name) {
                None => GenericType::TypeParameter(type_name),
                Some(entry) => entry.clone(),
            }
        }
        GenericType::NamedType(type_name, instantiation) => {
            let mut substituted_instantiation = Vec::new();

            for instantiated_type in instantiation {
                let substituted_type = substitute_type_parameter(instantiated_type, substitution);
                substituted_instantiation.push(substituted_type);
            }

            GenericType::NamedType(type_name, substituted_instantiation)
        }
        GenericType::NumberType => GenericType::NumberType,
    }
}

pub fn substitute_struct_fields<'a, 'b>(substitution: &'a SubstitutionMap<'b>, fields: &'a [GenericBinding<'b>]) -> Vec<GenericBinding<'b>> {
    let mut substituted_fields = Vec::new();

    for field_binding in fields.iter() {
        let substituted_field_type = substitute_type_parameter(&field_binding.type_, substitution);
        substituted_fields.push(GenericBinding { name: field_binding.name, type_: substituted_field_type });
    }

    substituted_fields
}

pub fn is_subtype_of<'a, 'b>(
    child_type: &'a GenericType<'b>,
    parent_type: &'a GenericType<'b>,
    type_environment: &'a TypeEnvironment<'b>,
    type_infos: &'a TypeInfos<'b>,
) -> Result<(), RufegoError> {
    match (child_type, parent_type) {
        (GenericType::NumberType, GenericType::NumberType) => return Ok(()),
        (GenericType::TypeParameter(child_name), GenericType::TypeParameter(parent_name)) => {
            if child_name == parent_name {
                return Ok(());
            }
            return Err(RufegoError { message: format!("Type parameter '{child_name}' can not be a subtype of type parameter '{parent_name}'") });
        }
        (GenericType::NamedType(child_name, _), GenericType::NamedType(parent_name, _)) => {
            let child_type_info = match type_infos.get(child_name) {
                None => {
                    return Err(RufegoError { message: format!("Child type '{child_name}' is not declared") });
                }
                Some(type_info) => {
                    type_info
                }
            };

            let parent_type_info = match type_infos.get(parent_name) {
                None => {
                    return Err(RufegoError { message: format!("Parent type '{parent_name}' is not declared") });
                }
                Some(type_info) => {
                    type_info
                }
            };

            match (child_type_info, parent_type_info) {
                (TypeInfo::Struct { .. }, TypeInfo::Struct { .. }) => {
                    if child_name == parent_name {
                        return Ok(());
                    }
                    return Err(RufegoError { message: format!("Struct type '{child_name}' can not be a subtype of struct type '{parent_name}'") });
                }
                (_, TypeInfo::Interface { .. }) => {
                    let child_methods = methods_of_type(child_type, type_environment, type_infos)?;
                    let parent_methods = methods_of_type(parent_type, type_environment, type_infos)?;

                    for parent_method in parent_methods.iter() {
                        match child_methods.iter().find(|method_spec| method_spec.name == parent_method.name) {
                            None => {
                                let error_message = format!(
                                    "Method '{}' of parent type '{parent_name}' is not implemented for child type '{child_name}'",
                                    parent_method.name
                                );
                                return Err(RufegoError { message: error_message });
                            }
                            Some(found_method_spec) => {
                                for (index, parent_parameter_binding) in parent_method.parameters.iter().enumerate() {
                                    let child_parameter_binding = found_method_spec.parameters.get(index).unwrap();

                                    is_subtype_of(&child_parameter_binding.type_, &parent_parameter_binding.type_, type_environment, type_infos)?;
                                }

                                is_subtype_of(&found_method_spec.return_type, &parent_method.return_type, type_environment, type_infos)?;
                            }
                        }
                    }
                }
                _ => {
                    return Err(RufegoError { message: format!("Type '{child_name}' can not be a subtype of struct type '{parent_name}'") });
                }
            }
        }
        (GenericType::TypeParameter(child_type_parameter), GenericType::NamedType(parent_name, _)) => {
            let parent_type_info = match type_infos.get(parent_name) {
                None => {
                    return Err(RufegoError { message: format!("Parent type '{parent_name}' is not declared") });
                }
                Some(type_info) => {
                    type_info
                }
            };

            match parent_type_info {
                TypeInfo::Struct { .. } => {
                    return Err(RufegoError { message: format!("Type parameter '{child_type_parameter}' can not be a subtype of struct type '{parent_name}'") });
                }
                TypeInfo::Interface { .. } => {
                    let child_methods = methods_of_type(child_type, type_environment, type_infos)?;
                    let parent_methods = methods_of_type(parent_type, type_environment, type_infos)?;

                    for parent_method in parent_methods.iter() {
                        if !child_methods.contains(parent_method) {
                            let error_message = format!("Method '{}' of parent type '{parent_name}' is not implemented for child type '{}'",
                                                        parent_method.name,
                                                        child_type.name()
                            );
                            return Err(RufegoError { message: error_message });
                        }
                    }
                }
            }
        }
        _ => return Err(RufegoError { message: format!("Child type '{}' is not a subtype of parent type '{}'", child_type.name(), parent_type.name()) })
    }

    Ok(())
}

pub fn methods_of_type<'a, 'b>(
    type_: &'a GenericType<'b>,
    type_environment: &'a TypeEnvironment<'b>,
    type_infos: &'a TypeInfos<'b>,
) -> Result<Vec<MethodSpecification<'b>>, RufegoError> {
    match type_ {
        GenericType::TypeParameter(type_parameter) => {
            return match type_environment.get(type_parameter) {
                None => {
                    Err(RufegoError { message: format!("Type parameter '{type_parameter}' is not declared in this context") })
                }
                Some(type_bound) => {
                    methods_of_type(type_bound, type_environment, type_infos)
                }
            };
        }
        GenericType::NamedType(type_name, instantiation) => {
            match type_infos.get(type_name) {
                None => {
                    Err(RufegoError { message: format!("Type '{type_name}' is not declared") })
                }
                Some(type_info) => {
                    match type_info {
                        TypeInfo::Struct { bound, methods, .. } => {
                            let substitution = generate_substitution_with_bound_check(bound, instantiation, type_environment, type_infos)?;
                            let mut substituted_method_specifications = Vec::new();

                            for method_declaration in methods.values() {
                                let substituted_method = substitute_method_specification(&method_declaration.specification, &substitution);
                                substituted_method_specifications.push(substituted_method);
                            }

                            Ok(substituted_method_specifications)
                        }
                        TypeInfo::Interface { bound, methods } => {
                            let substitution = generate_substitution(bound, instantiation)?;
                            let mut substituted_method_specifications = Vec::new();

                            for method_specification in methods.iter() {
                                let substituted_method_specification = substitute_method_specification(method_specification, &substitution);
                                substituted_method_specifications.push(substituted_method_specification);
                            }

                            Ok(substituted_method_specifications)
                        }
                    }
                }
            }
        }
        GenericType::NumberType => {
            Ok(Vec::new())
        }
    }
}

pub fn generate_substitution_with_bound_check<'a, 'b>(
    type_formals: &'a Vec<GenericBinding<'b>>,
    instantiation: &'a Vec<GenericType<'b>>,
    type_environment: &TypeEnvironment,
    type_infos: &TypeInfos,
) -> Result<SubstitutionMap<'b>, RufegoError> {
    let generated_substitution = generate_substitution(type_formals, instantiation)?;

    for (index, actual_type) in instantiation.iter().enumerate() {
        let bound = &type_formals.get(index).unwrap().type_;

        // substitute type parameter with actual types
        let bound = substitute_type_parameter(bound, &generated_substitution);

        is_subtype_of(actual_type, &bound, type_environment, type_infos)?;
    }

    Ok(generated_substitution)
}

pub fn substitute_method_specification<'a, 'b>(method_specification: &'a MethodSpecification<'b>, substitution: &'a SubstitutionMap<'b>) -> MethodSpecification<'b> {
    let mut substituted_bound = Vec::new();

    for binding in &method_specification.bound {
        let substituted_bound_parameter = substitute_type_parameter(&binding.type_, substitution);
        substituted_bound.push(GenericBinding { name: binding.name, type_: substituted_bound_parameter });
    }

    let mut substituted_method_parameters = Vec::new();

    for method_parameter in &method_specification.parameters {
        let substituted_method_parameter = substitute_type_parameter(&method_parameter.type_, substitution);
        substituted_method_parameters.push(GenericBinding { name: method_parameter.name, type_: substituted_method_parameter })
    }

    let substituted_return_type = substitute_type_parameter(&method_specification.return_type, substitution);

    MethodSpecification {
        name: method_specification.name,
        bound: substituted_bound,
        parameters: substituted_method_parameters,
        return_type: substituted_return_type,
    }
}

pub fn expression_well_formed<'a, 'b>(
    expression: &'a Expression<'b>,
    variable_environment: &'a VariableEnvironment<'b>,
    delta: &'a TypeEnvironment<'b>,
    type_infos: &'a TypeInfos<'b>,
) -> Result<GenericType<'b>, RufegoError> {
    match expression {
        Expression::Variable { name: variable_name } => {
            // variable declared in environment?
            match variable_environment.get(variable_name) {
                Some(var_type) => Ok(var_type.clone()),
                None => Err(RufegoError { message: format!("Current context does not have a variable with name '{variable_name}'") })
            }
        }
        Expression::MethodCall { expression, method, instantiation, parameter_expressions } => {
            let expression_type = expression_well_formed(expression, variable_environment, delta, type_infos)?;

            // retrieve all methods for the expression type
            let type_methods = methods_of_type(&expression_type, delta, type_infos)?;

            let method_specification = match type_methods.iter().find(|method_spec| &method_spec.name == method) {
                None => {
                    return Err(RufegoError { message: format!("Method '{method}' is not implemented for receiver type '{}'", expression_type.name()) });
                }
                Some(method_specification) => {
                    method_specification
                }
            };

            let parameter_substitution = generate_substitution_with_bound_check(
                &method_specification.bound,
                instantiation,
                delta,
                type_infos,
            )?;

            if method_specification.parameters.len() != parameter_expressions.len() {
                let error_message = format!(
                    "Method '{method}' for receiver type '{}' expects '{}' parameters but '{}' parameters were supplied",
                    expression_type.name(),
                    method_specification.parameters.len(),
                    parameter_expressions.len()
                );

                return Err(RufegoError { message: error_message });
            }

            for (index, parameter_expression) in parameter_expressions.iter().enumerate() {
                // eval and substitute parameter expression
                let actual_parameter_type = expression_well_formed(parameter_expression, variable_environment, delta, type_infos)?;
                let actual_parameter_type = substitute_type_parameter(&actual_parameter_type, &parameter_substitution);

                // substitute declared parameter type with instantiated types if possible
                let declared_parameter_type = &method_specification.parameters.get(index).unwrap().type_;
                let declared_parameter_type = substitute_type_parameter(declared_parameter_type, &parameter_substitution);

                // actual parameter type subtype from declared parameter type?
                match is_subtype_of(&actual_parameter_type, &declared_parameter_type, delta, type_infos) {
                    Ok(_) => {}
                    Err(error) => {
                        let error_message = format!(
                            "Parameter expression type '{}' is not a subtype of declared parameter type '{}':\n{}",
                            actual_parameter_type.name(),
                            declared_parameter_type.name(),
                            error.message
                        );
                        return Err(RufegoError { message: error_message });
                    }
                }
            }

            Ok(substitute_type_parameter(&method_specification.return_type, &parameter_substitution))
        }
        Expression::StructLiteral { name, instantiation, field_expressions } => {
            let struct_type = GenericType::NamedType(name, instantiation.clone());

            // struct type well-formed?
            match type_well_formed(&struct_type, delta, type_infos) {
                Ok(_) => {}
                Err(rufego_error) => {
                    return Err(RufegoError { message: format!("Struct literal '{name}' is not well-formed:\n{}", rufego_error.message) });
                }
            }

            let type_info = match type_infos.get(name) {
                None => {
                    return Err(RufegoError { message: format!("Struct literal '{name}' is not declared") });
                }
                Some(type_info) => {
                    type_info
                }
            };

            match type_info {
                TypeInfo::Struct { bound, fields, .. } => {
                    if fields.len() != field_expressions.len() {
                        let error_message = format!(
                            "Struct type '{name}' has '{}' fields but '{}' field values were provided",
                            fields.len(),
                            field_expressions.len()
                        );
                        return Err(RufegoError { message: error_message });
                    }

                    let mut field_expression_types = Vec::new();

                    // evaluate field expressions
                    for expression in field_expressions.iter() {
                        let field_expression_type = expression_well_formed(expression, variable_environment, delta, type_infos)?;
                        field_expression_types.push(field_expression_type);
                    }

                    // generate substitution map for struct fields
                    let substitution = generate_substitution(bound, instantiation)?;

                    // substitute fields with type parameters with their instantiation
                    let substituted_struct_fields = substitute_struct_fields(&substitution, fields);

                    for (index, substituted_field) in substituted_struct_fields.iter().enumerate() {
                        let expression_type = field_expression_types.get(index).unwrap();

                        match is_subtype_of(expression_type, &substituted_field.type_, delta, type_infos) {
                            Ok(_) => {}
                            Err(rufego_error) => {
                                let error_message = format!("Expression for field '{}' of struct literal '{}' is not a subtype of declared field type:\n{}",
                                                            substituted_field.name,
                                                            name,
                                                            rufego_error.message);
                                return Err(RufegoError { message: error_message });
                            }
                        }
                    }

                    Ok(struct_type)
                }
                TypeInfo::Interface { .. } => Err(RufegoError { message: format!("Constructed literal '{name}' is an interface type") }),
            }
        }
        Expression::Select { expression, field: field_var } => {
            let expression_type = expression_well_formed(expression, variable_environment, delta, type_infos)?;

            if let GenericType::NamedType(type_name, instantiation) = expression_type {
                return match type_infos.get(type_name).unwrap() {
                    TypeInfo::Struct { bound, fields, .. } => {
                        // generate substitution map for struct fields
                        let substitution = generate_substitution(bound, &instantiation)?;

                        // substitute fields with type parameters with their instantiation
                        let substituted_struct_fields = substitute_struct_fields(&substitution, fields);

                        for field_binding in &substituted_struct_fields {
                            if &field_binding.name == field_var {
                                return Ok(field_binding.type_.clone());
                            }
                        }

                        Err(RufegoError { message: format!("Struct type '{type_name}' does not have a field named '{field_var}'") })
                    }
                    TypeInfo::Interface { .. } => {
                        Err(RufegoError { message: format!("Select expression evaluates to interface type '{type_name}'") })
                    }
                };
            }

            Err(RufegoError { message: format!("Select expression are only allowed on named types, expression evaluated to '{}' instead", expression_type.name()) })
        }
        Expression::TypeAssertion { expression, assert } => {
            // evaluate body expression
            let expression_type = expression_well_formed(expression, variable_environment, delta, type_infos)?;

            // asserted type declared?
            type_well_formed(assert, delta, type_infos)?;

            match expression_type {
                GenericType::TypeParameter(_) => {
                    match type_infos.get(assert.name()).unwrap() {
                        TypeInfo::Struct { .. } => {
                            let bounds_of_asserted_type = bounds_of_type(&expression_type, type_infos, delta)?;
                            is_subtype_of(assert, bounds_of_asserted_type, delta, type_infos)?;
                        }
                        TypeInfo::Interface { .. } => {}
                    }
                }
                GenericType::NamedType(type_name, _) => {
                    match type_infos.get(type_name).unwrap() {
                        TypeInfo::Struct { .. } => {}
                        TypeInfo::Interface { .. } => {
                            match type_infos.get(assert.name()).unwrap() {
                                TypeInfo::Struct { .. } => {
                                    let bounds_of_asserted_type = bounds_of_type(&expression_type, type_infos, delta)?;
                                    is_subtype_of(assert, bounds_of_asserted_type, delta, type_infos)?;
                                }
                                TypeInfo::Interface { .. } => {}
                            }
                        }
                    }
                }
                GenericType::NumberType => {
                    match assert {
                        GenericType::NumberType => {}
                        _ => {
                            return Err(RufegoError { message: format!("Type assertion is not well-formed\nTried to assert '{}' on type '{}'", assert.name(), expression_type.name()) });
                        }
                    }
                }
            }

            Ok(assert.clone())
        }
        Expression::Number { .. } => {
            Ok(GenericType::NumberType)
        }
        Expression::BinOp { lhs, rhs, .. } => {
            // left side of operation number type?
            let lhs_type = expression_well_formed(lhs, variable_environment, delta, type_infos)?;

            // right side of operation number type?
            let rhs_type = expression_well_formed(rhs, variable_environment, delta, type_infos)?;

            match (&lhs_type, &rhs_type) {
                (GenericType::NumberType, GenericType::NumberType) => Ok(GenericType::NumberType),
                _ => Err(RufegoError { message: format!("Binary operation is not well-formed:\nLHS was type '{}', RHS was type '{}'", lhs_type.name(), rhs_type.name()) })
            }
        }
    }
}

pub fn bounds_of_type<'a>(type_: &'a GenericType, type_infos: &'a TypeInfos, delta: &'a TypeEnvironment) -> Result<&'a GenericType<'a>, RufegoError> {
    match type_ {
        GenericType::TypeParameter(type_parameter) => {
            match delta.get(type_parameter) {
                None => Err(RufegoError { message: format!("Type parameter '{type_parameter}' is not declared in this context") }),
                Some(type_bound) => Ok(type_bound)
            }
        }
        GenericType::NamedType(name, _) => {
            match type_infos.get(name) {
                None => Err(RufegoError { message: format!("Type '{name}' is not declared") }),
                Some(_) => Ok(type_)
            }
        }
        GenericType::NumberType => Ok(type_)
    }
}

pub fn type_well_formed(type_: &GenericType, delta: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), RufegoError> {
    match type_ {
        GenericType::TypeParameter(type_parameter) => {
            type_parameter_well_formed(type_parameter, delta)?
        }
        GenericType::NamedType(type_name, instantiation) => {
            type_actual_well_formed(instantiation, delta, type_infos)?;

            // instantiated types satisfy type bounds of type formals?
            let type_info = match type_infos.get(type_name) {
                None => {
                    return Err(RufegoError { message: format!("Type '{type_name}' is not declared in this context") });
                }
                Some(type_info) => {
                    type_info
                }
            };

            match type_info {
                TypeInfo::Struct { bound, .. } => {
                    let _ = generate_substitution_with_bound_check(bound, instantiation, delta, type_infos)?;
                }
                TypeInfo::Interface { bound, .. } => {
                    let _ = generate_substitution_with_bound_check(bound, instantiation, delta, type_infos)?;
                }
            }
        }
        GenericType::NumberType => (),
    }

    Ok(())
}

pub fn type_parameter_well_formed(type_parameter: &str, type_environment: &TypeEnvironment) -> Result<(), RufegoError> {
    match type_environment.get(type_parameter) {
        None => Err(RufegoError { message: format!("Type parameter '{type_parameter}' is not declared in this context") }),
        Some(_) => Ok(())
    }
}

pub fn type_actual_well_formed(instantiation: &Vec<GenericType>, type_environment: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), RufegoError> {
    // instantiation types well-formed?
    for instantiated_type in instantiation {
        type_well_formed(instantiated_type, type_environment, type_infos)?;
    }

    Ok(())
}

pub fn create_type_infos<'a>(program: &'a Program<'a>) -> Result<TypeInfos, RufegoError> {
    let mut type_infos = HashMap::new();

    // collect info for type declarations
    for declaration in &program.declarations {
        if let Declaration::Type { name: type_name, bound, literal } = declaration {
            // type with this name already declared?
            if type_infos.contains_key(type_name) {
                let error_message = format!("Type declaration '{type_name}' is not well-formed: Type '{type_name}' was already declared");
                return Err(RufegoError { message: error_message });
            } else {
                let type_info = match literal {
                    TypeLiteral::Struct { fields } => TypeInfo::Struct { bound, fields, methods: HashMap::new() },
                    TypeLiteral::Interface { methods } => TypeInfo::Interface { bound, methods },
                };

                type_infos.insert(*type_name, type_info);
            }
        }
    }

    // collect info for method declarations
    for declaration in &program.declarations {
        if let Declaration::Method(method) = declaration {
            match type_infos.get_mut(method.receiver.type_) {
                Some(TypeInfo::Interface { .. }) => {
                    let error_message = format!("Method declaration '{}' is not well-formed:\nProvided receiver type '{}' is an interface",
                                                method.specification.name,
                                                method.receiver.type_
                    );
                    return Err(RufegoError { message: error_message });
                }
                Some(TypeInfo::Struct { methods, .. }) => {
                    // is the method already declared?
                    if methods.insert(method.specification.name, method).is_some() {
                        let error_message = format!(
                            "Method declaration '{}' is not well-formed:\nMethod is already declared for receiver type '{}'",
                            method.specification.name,
                            method.receiver.type_
                        );
                        return Err(RufegoError { message: error_message });
                    }
                }
                None => {
                    let error_message = format!(
                        "Method declaration '{}' is not well-formed:\nProvided receiver type '{}' is not declared",
                        method.specification.name,
                        method.receiver.type_
                    );
                    return Err(RufegoError { message: error_message });
                }
            }
        }
    }

    Ok(type_infos)
}