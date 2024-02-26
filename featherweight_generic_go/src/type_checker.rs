use std::collections::{HashMap};
use parser::{Declaration, Expression, GenericBinding, GenericReceiver, GenericType, MethodDeclaration, MethodSpecification, Program, TypeLiteral};

// TODO Self recursion in struct
// TODO Präsi (Wann, Inhalt, Ablauf, Gespräch danach?)

// Type name -> Type Info
pub(crate) type TypeInfos<'a> = HashMap<&'a str, TypeInfo<'a>>;

// Type parameter -> Bound
pub(crate) type TypeEnvironment<'a> = HashMap<&'a str, GenericType<'a>>;

// Variables -> Type
pub(crate) type VariableEnvironment<'a> = HashMap<&'a str, GenericType<'a>>;

// Type parameter -> Actual type instantiation
pub(crate) type SubstitutionMap<'a> = HashMap<&'a str, GenericType<'a>>;

#[derive(Eq, PartialEq, Debug, Clone)]
pub(crate) enum TypeInfo<'a> {
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

#[derive(Debug)]
pub(crate) struct TypeError {
    pub(crate) message: String,
}

pub(crate) fn create_type_infos<'a>(program: &'a Program<'a>) -> Result<TypeInfos, TypeError> {
    let mut type_infos = HashMap::new();

    // collect info for type declarations
    for declaration in &program.declarations {
        if let Declaration::Type { name: type_name, bound, literal } = declaration {
            // type with this name already declared?
            if type_infos.contains_key(type_name) {
                let error_message = format!("Type declaration '{type_name}' is not well-formed: Type '{type_name}' was already declared");
                return Err(TypeError { message: error_message });
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
                    let error_message = format!("Method declaration '{}' is not well-formed: Provided receiver type '{}' is an interface",
                                                method.specification.name,
                                                method.receiver.type_
                    );
                    return Err(TypeError { message: error_message });
                }
                Some(TypeInfo::Struct { methods, .. }) => {
                    // is the method already declared?
                    if methods.insert(method.specification.name, method).is_some() {
                        let error_message = format!("Method declaration '{}' is not well-formed: Method is already declared for receiver type '{}'",
                                                    method.specification.name,
                                                    method.receiver.type_
                        );
                        return Err(TypeError { message: error_message });
                    }
                }
                None => {
                    let error_message = format!("Method declaration '{}' is not well-formed: Provided receiver type '{}' is not declared",
                                                method.specification.name,
                                                method.receiver.type_
                    );
                    return Err(TypeError { message: error_message });
                }
            }
        }
    }

    Ok(type_infos)
}

pub(crate) fn program_well_formed<'a>(program: &'a Program<'a>, type_infos: &TypeInfos<'a>) -> Result<GenericType<'a>, TypeError> {
    // declarations well-formed?
    for declaration in &program.declarations {
        declaration_well_formed(declaration, type_infos)?;
    }

    // body expression well-formed in the empty type environment and empty variable environment?
    match expression_well_formed(&program.expression, &HashMap::new(), &HashMap::new(), type_infos) {
        Ok(type_) => Ok(type_),
        Err(error) => {
            Err(TypeError { message: format!("Body expression of 'main' is not well-formed:\n{}", error.message) })
        }
    }
}

fn declaration_well_formed(declaration: &Declaration, type_infos: &TypeInfos) -> Result<(), TypeError> {
    match declaration {
        // method declaration well-formed?
        Declaration::Method(MethodDeclaration { receiver, specification, body }) => {
            match method_well_formed(receiver, specification, body, type_infos) {
                Ok(_) => {}
                Err(error) => {
                    let error_message = format!("Method '{}' for receiver type '{}' is not well-formed:\n{}",
                                                specification.name,
                                                receiver.type_,
                                                error.message
                    );
                    return Err(TypeError { message: error_message });
                }
            }
        }
        // type declaration well-formed?
        Declaration::Type { name, bound, literal } => {
            let mut psi = HashMap::new();

            // build environment for type formals of literal
            for binding in bound.iter() {
                psi.insert(binding.name, binding.type_.clone());
            }

            match formal_type_well_formed(&HashMap::new(), &psi, type_infos) {
                Ok(_) => {}
                Err(error) => {
                    return Err(TypeError { message: format!("Type formals of literal '{name}' is not well-formed: {}", error.message) });
                }
            }

            // type literal well-formed?
            match type_literal_well_formed(literal, &psi, type_infos) {
                Ok(_) => {}
                Err(error) => {
                    return Err(TypeError { message: format!("Literal '{name}' is not well-formed: {}", error.message) });
                }
            }
        }
    }

    Ok(())
}

fn type_literal_well_formed(literal: &TypeLiteral, literal_environment: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), TypeError> {
    match literal {
        TypeLiteral::Struct { fields } => struct_well_formed(fields, literal_environment, type_infos)?,
        TypeLiteral::Interface { methods } => interface_well_formed(methods, literal_environment, type_infos)?,
    }

    Ok(())
}

fn struct_well_formed(fields: &[GenericBinding], type_environment: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), TypeError> {
    for (index, field) in fields.iter().enumerate() {
        // field names distinct?
        if fields.iter().skip(index + 1).any(|binding| binding.name == field.name) {
            return Err(TypeError { message: format!("Duplicate field with name '{}'", field.name) });
        }

        // field type well-formed in the literal environment?
        type_well_formed(&field.type_, type_environment, type_infos)?;
    }

    Ok(())
}

fn interface_well_formed(methods: &[MethodSpecification], type_environment: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), TypeError> {
    for (index, method_specification) in methods.iter().enumerate() {
        // name of method specification unique?
        if methods.iter().skip(index + 1).any(|method_spec| method_spec.name == method_specification.name) {
            return Err(TypeError { message: format!("Duplicate method specification with name '{}'", method_specification.name) });
        }

        // method specification well-formed in the literal environment?
        match method_specification_well_formed(method_specification, type_environment, type_infos) {
            Ok(_) => {}
            Err(error) => {
                return Err(TypeError { message: format!("Method specification '{}' is not well-formed: {}", method_specification.name, error.message) });
            }
        }
    }

    Ok(())
}

fn method_specification_well_formed(
    specification: &MethodSpecification,
    literal_environment: &TypeEnvironment,
    type_infos: &TypeInfos,
) -> Result<(), TypeError> {
    // build environment for type formals of method specification
    let mut method_environment = HashMap::new();

    for binding in &specification.bound {
        method_environment.insert(binding.name, binding.type_.clone());
    }

    // concatenate type environment of literal and method
    let delta = nested_type_formals_well_formed(literal_environment, &method_environment, type_infos)?;

    for (index, parameter) in specification.parameters.iter().enumerate() {
        // name of method parameter distinct?
        if specification.parameters.iter().skip(index + 1).any(|element| element.name == parameter.name) {
            return Err(TypeError { message: format!("Duplicate method parameter with name '{}'", parameter.name) });
        }

        // parameter type well-formed in the concatenated environment?
        match type_well_formed(&parameter.type_, &delta, type_infos) {
            Ok(_) => {}
            Err(error) => {
                let error_msg = format!("Method parameter '{}' with type '{}' is not well-formed: {}",
                                        parameter.name,
                                        parameter.type_.name(),
                                        error.message
                );

                return Err(TypeError { message: error_msg });
            }
        }
    }

    // return type well-formed in the concatenated environment?
    match type_well_formed(&specification.return_type, &delta, type_infos) {
        Ok(_) => {}
        Err(error) => {
            return Err(TypeError { message: format!("Return type '{}' is not well-formed: {}", specification.return_type.name(), error.message) });
        }
    }

    Ok(())
}

// TODO generic return type of interface --> how to implement by struct?

fn method_well_formed(receiver: &GenericReceiver, specification: &MethodSpecification, body: &Expression, type_infos: &TypeInfos) -> Result<(), TypeError> {
    for (index, parameter) in specification.parameters.iter().enumerate() {
        // name of receiver type and parameter distinct?
        if receiver.name == parameter.name || specification.parameters.iter().skip(index + 1).any(|binding| binding.name == parameter.name) {
            return Err(TypeError { message: format!("Duplicate method parameter with name '{}'", parameter.name) });
        }
    }

    // receiver type declared?
    match type_infos.get(receiver.type_) {
        Some(_) => {
            // build environment for type formals of receiver type
            let mut receiver_environment = HashMap::new();

            // keep track of instantiated types
            let mut instantiated_types = Vec::new();

            for binding in &receiver.instantiation {
                receiver_environment.insert(binding.name, binding.type_.clone());
                instantiated_types.push(GenericType::TypeParameter(binding.name));
            }

            // create receiver type with instantiated types
            let receiver_type = GenericType::NamedType(receiver.type_, instantiated_types);

            // build environment for type formals of method
            let mut method_environment = HashMap::new();

            for binding in &specification.bound {
                method_environment.insert(binding.name, binding.type_.clone());
            }

            // concatenate environment of receiver type and method
            let delta = nested_type_formals_well_formed(&receiver_environment, &method_environment, type_infos)?;

            // parameter types well-formed in the concatenated environment?
            for parameter in &specification.parameters {
                type_well_formed(&parameter.type_, &delta, type_infos)?;
            }

            // return-type well-formed in the concatenated environment??
            type_well_formed(&specification.return_type, &delta, type_infos)?;

            // create environment for method parameters
            let mut variable_environment = HashMap::new();

            variable_environment.insert(receiver.name, receiver_type);

            for parameter in &specification.parameters {
                variable_environment.insert(parameter.name, parameter.type_.clone());
            }

            // body expression well-formed in the concatenated environment?
            match expression_well_formed(body, &variable_environment, &delta, type_infos) {
                Ok(expression_type) => {
                    // body expression subtype of return type in the concatenated environment?
                    match is_subtype_of(&expression_type, &specification.return_type, &delta, type_infos) {
                        Ok(_) => {}
                        Err(error) => {
                            let error_message = format!("Body expression type '{}' is not a subtype of declared return type '{}':\n{}",
                                                        expression_type.name(),
                                                        specification.return_type.name(),
                                                        error.message
                            );
                            return Err(TypeError { message: error_message });
                        }
                    }
                }
                Err(error) => {
                    return Err(TypeError { message: format!("Body expression of method is not well-formed:\n{}", error.message) });
                }
            }
        }
        None => {
            return Err(TypeError { message: format!("Receiver type '{}' is not declared", receiver.type_) });
        }
    }

    Ok(())
}

fn type_well_formed(type_: &GenericType, delta: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), TypeError> {
    match type_ {
        GenericType::TypeParameter(type_parameter) => {
            type_parameter_well_formed(type_parameter, delta)?
        }
        GenericType::NamedType(type_name, instantiation) => {
            type_actual_well_formed(instantiation, delta, type_infos)?;

            // instantiated types satisfy type bounds of type formals?
            match type_infos.get(type_name) {
                None => return Err(TypeError { message: format!("Type '{type_name}' is not declared in this context") }),
                Some(type_info) => {
                    match type_info {
                        TypeInfo::Struct { bound, .. } => {
                            let _ = generate_substitution_with_bound_check(bound, instantiation, delta, type_infos)?;
                        }
                        TypeInfo::Interface { bound, .. } => {
                            let _ = generate_substitution_with_bound_check(bound, instantiation, delta, type_infos)?;
                        }
                    }
                }
            }
        }
        GenericType::NumberType => (),
    }

    Ok(())
}

fn type_parameter_well_formed(type_parameter: &str, type_environment: &TypeEnvironment) -> Result<(), TypeError> {
    match type_environment.get(type_parameter) {
        None => Err(TypeError { message: format!("Type parameter '{type_parameter}' is not declared in this context") }),
        Some(_) => Ok(())
    }
}

fn type_actual_well_formed(instantiation: &Vec<GenericType>, type_environment: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), TypeError> {
    // instantiation types well-formed?
    for instantiated_type in instantiation {
        type_well_formed(instantiated_type, type_environment, type_infos)?;
    }

    Ok(())
}

fn formal_type_well_formed<'a>(outer: &TypeEnvironment<'a>, inner: &TypeEnvironment<'a>, type_infos: &TypeInfos) -> Result<TypeEnvironment<'a>, TypeError> {
    let mut concat_environment = HashMap::new();

    // insert types of outer environment to concatenated type environment
    for (name, type_) in outer.iter() {
        concat_environment.insert(*name, type_.clone());
    }

    for (name, type_) in inner.iter() {
        // duplicate type parameter names?
        match concat_environment.insert(*name, type_.clone()) {
            Some(_) => return Err(TypeError { message: format!("Duplicate type parameter with name '{name}'") }),
            None => continue,
        }
    }

    // types of the inner environment well-formed in the concatenated environment?
    for type_ in inner.values() {
        type_well_formed(type_, &concat_environment, type_infos)?;
    }

    Ok(concat_environment)
}

fn nested_type_formals_well_formed<'a>(outer: &TypeEnvironment<'a>, inner: &TypeEnvironment<'a>, type_infos: &TypeInfos) -> Result<TypeEnvironment<'a>, TypeError> {
    let _ = formal_type_well_formed(&HashMap::new(), outer, type_infos)?;
    let delta = formal_type_well_formed(outer, inner, type_infos)?;

    Ok(delta)
}

pub(crate) fn expression_well_formed<'a>(
    expression: &'a Expression<'a>,
    variable_environment: &VariableEnvironment<'a>,
    delta: &TypeEnvironment<'a>,
    type_infos: &TypeInfos<'a>,
) -> Result<GenericType<'a>, TypeError> {
    match expression {
        Expression::Variable { name: variable_name } => {
            // variable declared in environment?
            match variable_environment.get(variable_name) {
                Some(var_type) => Ok(var_type.clone()),
                None => Err(TypeError { message: format!("Current context does not have a variable with name '{variable_name}'") })
            }
        }
        Expression::MethodCall { expression, method, instantiation, parameter_expressions } => {
            let expression_type = expression_well_formed(expression, variable_environment, delta, type_infos)?;

            // TODO 2 Times substitution?            
            match expression_type {
                GenericType::TypeParameter(_) => {
                    todo!()
                }
                GenericType::NamedType(type_name, type_instantiation) => {
                    match type_infos.get(type_name).unwrap() {
                        TypeInfo::Struct { bound, methods, .. } => {
                            match methods.get(method) {
                                None => Err(TypeError { message: format!("Method '{method}' is not implemented for type '{type_name}'") }),
                                Some(declaration) => {
                                    // TODO verify again...
                                    let method_spec_substitution = generate_substitution_with_bound_check(bound, &type_instantiation, delta, type_infos)?;
                                    let substituted_method_specification = substitute_method_specification(&declaration.specification, &method_spec_substitution)?;

                                    let method_parameters_substitution = generate_substitution_with_bound_check(&declaration.specification.bound, instantiation, delta, type_infos)?;

                                    for (index, parameter_expression) in parameter_expressions.iter().enumerate() {
                                        let parameter_expression_type = expression_well_formed(parameter_expression, variable_environment, delta, type_infos)?;
                                        let substituted_parameter_expression = substitute_type_parameter(&parameter_expression_type, &method_parameters_substitution);

                                        let substituted_method_spec_parameter = &substituted_method_specification.parameters.get(index).unwrap().type_;

                                        match is_subtype_of(&substituted_parameter_expression, substituted_method_spec_parameter, delta, type_infos) {
                                            Ok(_) => {}
                                            Err(error) => {
                                                let error_message = format!("Parameter expression type '{}' is not a subtype of declared parameter type '{}':\n{}",
                                                                            substituted_parameter_expression.name(),
                                                                            substituted_method_spec_parameter.name(),
                                                                            error.message
                                                );
                                                return Err(TypeError { message: error_message });
                                            }
                                        }
                                    }

                                    Ok(substitute_type_parameter(&substituted_method_specification.return_type, &method_parameters_substitution))
                                }
                            }
                        }
                        TypeInfo::Interface { .. } => {
                            todo!()
                        }
                    }
                }
                GenericType::NumberType => Err(TypeError { message: format!("Method '{method}' cannot be called on a number type") })
            }
        }
        Expression::StructLiteral { name, instantiation, field_expressions } => {
            let struct_type = GenericType::NamedType(name, instantiation.clone());

            // struct type well-formed?
            type_well_formed(&struct_type, delta, type_infos)?;

            match type_infos.get(name) {
                Some(type_info) => {
                    match type_info {
                        TypeInfo::Struct { bound, fields, .. } => {
                            if fields.len() != field_expressions.len() {
                                let error_message = format!("Struct type '{name}' has '{}' fields but '{}' field values were provided",
                                                            fields.len(),
                                                            field_expressions.len()
                                );
                                return Err(TypeError { message: error_message });
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
                            let substituted_struct_fields = substitute_struct_fields(&substitution, fields)?;

                            for (index, substituted_field) in substituted_struct_fields.iter().enumerate() {
                                let expression_type = field_expression_types.get(index).unwrap();

                                match is_subtype_of(expression_type, &substituted_field.type_, delta, type_infos) {
                                    Ok(_) => {}
                                    Err(error) => {
                                        let error_message = format!("Expression for field '{}' of struct literal '{}' is not a subtype of declared field type:\n{}",
                                                                    substituted_field.name,
                                                                    name,
                                                                    error.message);
                                        return Err(TypeError { message: error_message });
                                    }
                                }
                            }

                            Ok(struct_type)
                        }
                        TypeInfo::Interface { .. } => Err(TypeError { message: format!("Constructed literal '{name}' is an interface type") }),
                    }
                }
                None => Err(TypeError { message: format!("Struct literal '{name}' is not declared") }),
            }
        }
        Expression::Select { expression, field: field_var } => {
            let expression_type = expression_well_formed(expression, variable_environment, delta, type_infos)?;

            match expression_type {
                GenericType::NamedType(type_name, instantiation) => {
                    match type_infos.get(type_name).unwrap() {
                        TypeInfo::Struct { bound, fields, .. } => {
                            // generate substitution map for struct fields
                            let substitution = generate_substitution(bound, &instantiation)?;

                            // substitute fields with type parameters with their instantiation
                            let substituted_struct_fields = substitute_struct_fields(&substitution, fields)?;

                            for field_binding in &substituted_struct_fields {
                                if &field_binding.name == field_var {
                                    return Ok(field_binding.type_.clone());
                                }
                            }

                            Err(TypeError { message: format!("Struct type '{type_name}' does not have a field named '{field_var}'") })
                        }
                        TypeInfo::Interface { .. } => {
                            Err(TypeError { message: format!("Select expression evaluates to interface type '{type_name}'") })
                        }
                    }
                }

                //_ => Err(TypeError { message: String::from("Select-expression evaluated to a number type or type parameter which is forbidden") }),
                GenericType::TypeParameter(type_parameter) => {
                    Err(TypeError { message: format!("Select expression evaluates to type parameter '{type_parameter}'") })
                }
                GenericType::NumberType => {
                    Err(TypeError { message: String::from("Select expression evaluates to a number type") })
                }
            }
        }
        Expression::TypeAssertion { expression, assert } => {
            // TODO variable as assert type?

            // asserted type well-formed?
            type_well_formed(assert, delta, type_infos)?;

            // evaluate body expression
            let expression_type = expression_well_formed(expression, variable_environment, delta, type_infos)?;

            match expression_type {
                GenericType::TypeParameter(type_parameter) => {
                    let type_info = typeinfo_of_interface_like_type(type_parameter, type_infos, delta)?;

                    match type_info {
                        TypeInfo::Struct { .. } => Ok(assert.clone()),
                        TypeInfo::Interface { .. } => {
                            let assert_type_info = typeinfo_of_interface_like_type(assert.name(), type_infos, delta)?;

                            match assert_type_info {
                                TypeInfo::Struct { .. } => {
                                    match bounds_of_type(&expression_type, type_infos, delta) {
                                        Ok(expression_bound) => {
                                            match is_subtype_of(assert, expression_bound, delta, type_infos) {
                                                Ok(_) => {}
                                                Err(error) => {
                                                    let error_message = format!("Asserted type '{}' is not a subtype of type bounds of expression type '{}':\n{}",
                                                                                assert.name(),
                                                                                expression_bound.name(),
                                                                                error.message
                                                    );
                                                    return Err(TypeError { message: error_message });
                                                }
                                            }

                                            Ok(assert.clone())
                                        }
                                        Err(error) => {
                                            let error_message = format!("Type bounds of expression type '{}' are not satisfied:\n{}",
                                                                        expression_type.name(),
                                                                        error.message
                                            );
                                            Err(TypeError { message: error_message })
                                        }
                                    }
                                }
                                TypeInfo::Interface { .. } => Ok(assert.clone())
                            }
                        }
                    }
                }
                GenericType::NamedType(type_name, _) => {
                    let type_info = typeinfo_of_interface_like_type(type_name, type_infos, delta)?;

                    match type_info {
                        TypeInfo::Struct { .. } => Ok(assert.clone()),
                        TypeInfo::Interface { .. } => {
                            let assert_type_info = typeinfo_of_interface_like_type(assert.name(), type_infos, delta)?;

                            match assert_type_info {
                                TypeInfo::Struct { .. } => {
                                    let expression_bound = bounds_of_type(&expression_type, type_infos, delta)?;

                                    is_subtype_of(assert, expression_bound, delta, type_infos)?;

                                    Ok(assert.clone())
                                }
                                TypeInfo::Interface { .. } => Ok(assert.clone())
                            }
                        }
                    }
                }
                GenericType::NumberType => {
                    match assert {
                        GenericType::NumberType => Ok(assert.clone()),
                        _ => Err(TypeError { message: format!("Assertion failed: Tried to assert '{}' on type '{}'", assert.name(), expression_type.name()) })
                    }
                }
            }
        }
        Expression::Number { .. } => {
            Ok(GenericType::NumberType)
        }
        Expression::BinOp { lhs, rhs, .. } => {
            // left side of operation number type?
            let lhs_type = expression_well_formed(lhs, variable_environment, delta, type_infos)?;

            // right side of operation number type?
            let rhs_type = expression_well_formed(rhs, variable_environment, delta, type_infos)?;

            match (lhs_type, rhs_type) {
                (GenericType::NumberType, GenericType::NumberType) => Ok(GenericType::NumberType),
                _ => Err(TypeError { message: String::from("Either LFS or RHS of a binary operation does not evaluate to a number type") })
            }
        }
    }
}

fn bounds_of_type<'a>(type_: &'a GenericType, type_infos: &'a TypeInfos, delta: &'a TypeEnvironment) -> Result<&'a GenericType<'a>, TypeError> {
    match type_ {
        GenericType::TypeParameter(type_parameter) => {
            match delta.get(type_parameter) {
                None => Err(TypeError { message: format!("Type parameter '{type_parameter}' is not declared in this context") }),
                Some(type_bound) => Ok(type_bound)
            }
        }
        GenericType::NamedType(name, _) => {
            match type_infos.get(name) {
                None => Err(TypeError { message: format!("Type '{name}' is not declared") }),
                Some(_) => Ok(type_)
            }
        }
        GenericType::NumberType => Ok(type_)
    }
}

fn typeinfo_of_interface_like_type<'a>(type_name: &str, type_infos: &'a TypeInfos, delta: &TypeEnvironment) -> Result<&'a TypeInfo<'a>, TypeError> {
    match type_infos.get(type_name) {
        None => {
            match delta.get(type_name) {
                None => Err(TypeError { message: format!("Could not find typeinfo for type '{type_name}'") }),
                Some(type_) => Ok(type_infos.get(type_.name()).unwrap()),
            }
        }
        Some(type_info) => Ok(type_info),
    }
}

pub(crate) fn generate_substitution<'a, 'b>(type_formals: &'a Vec<GenericBinding<'b>>, instantiation: &'a Vec<GenericType<'b>>) -> Result<SubstitutionMap<'b>, TypeError> {
    let mut generated_substitution = HashMap::new();

    // correct amount of type parameters supplied?
    if type_formals.len() != instantiation.len() {
        return Err(TypeError { message: format!("Type formals consists of '{}' parameters but '{}' parameters were provided", type_formals.len(), instantiation.len()) });
    }

    // substitute type formals with type actuals and check bound
    for (index, formal_type) in type_formals.iter().enumerate() {
        let actual_type = instantiation.get(index).unwrap();
        generated_substitution.insert(formal_type.name, actual_type.clone());
    }

    Ok(generated_substitution)
}

fn generate_substitution_with_bound_check<'a, 'b>(
    type_formals: &'a Vec<GenericBinding<'b>>,
    instantiation: &'a Vec<GenericType<'b>>,
    type_environment: &TypeEnvironment,
    type_infos: &TypeInfos,
) -> Result<SubstitutionMap<'b>, TypeError> {
    let generated_substitution = generate_substitution(type_formals, instantiation)?;

    for (index, actual_type) in instantiation.iter().enumerate() {
        let bound = &type_formals.get(index).unwrap().type_;

        // substitute type parameter with actual types
        let bound = substitute_type_parameter(bound, &generated_substitution);

        is_subtype_of(actual_type, &bound, type_environment, type_infos)?;
    }

    Ok(generated_substitution)
}

fn substitute_type_parameter<'a, 'b>(type_: &'a GenericType<'b>, substitution: &'a SubstitutionMap<'b>) -> GenericType<'b> {
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

pub(crate) fn substitute_struct_fields<'a, 'b>(substitution: &'a SubstitutionMap<'b>, fields: &'a [GenericBinding<'b>]) -> Result<Vec<GenericBinding<'b>>, TypeError> {
    let mut substituted_fields = Vec::new();

    for field_binding in fields.iter() {
        let substituted_field_type = substitute_type_parameter(&field_binding.type_, substitution);
        substituted_fields.push(GenericBinding { name: field_binding.name, type_: substituted_field_type });
    }

    Ok(substituted_fields)
}

fn substitute_method_specification<'a, 'b>(method_specification: &'a MethodSpecification<'b>, substitution: &'a SubstitutionMap<'b>) -> Result<MethodSpecification<'b>, TypeError> {
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

    Ok(MethodSpecification {
        name: method_specification.name,
        bound: substituted_bound,
        parameters: substituted_method_parameters,
        return_type: substituted_return_type,
    })
}

/*
    Checks if child_type is a subtype of parent_type
 */
pub(crate) fn is_subtype_of(child_type: &GenericType, parent_type: &GenericType, type_environment: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), TypeError> {
    match (child_type, parent_type) {
        (GenericType::NumberType, GenericType::NumberType) => return Ok(()),
        (GenericType::TypeParameter(child_name), GenericType::TypeParameter(parent_name)) => {
            if child_name == parent_name {
                return Ok(());
            }
            return Err(TypeError { message: format!("Type parameter '{child_name}' can not be a subtype of type parameter '{parent_name}'") });
        }
        (GenericType::NamedType(child_name, _), GenericType::NamedType(parent_name, _)) => {
            let child_type_info = type_infos.get(child_name).unwrap();
            let parent_type_info = type_infos.get(parent_name).unwrap();

            match (child_type_info, parent_type_info) {
                (TypeInfo::Struct { .. }, TypeInfo::Struct { .. }) => {
                    if child_name == parent_name {
                        return Ok(())
                    }
                    return Err(TypeError { message: format!("Struct type '{child_name}' can not be a subtype of struct type '{parent_name}'")})
                }
                (_, TypeInfo::Interface { .. }) => {
                    let child_methods = methods_of_type(child_type, type_environment, type_infos)?;
                    let parent_methods = methods_of_type(parent_type, type_environment, type_infos)?;
                    
                    println!("Child methods {:#?}", child_methods);
                    println!("Parent methods {:#?}", parent_methods);
                }
                _ => {
                    return Err(TypeError {message: format!("Type '{child_name}' can not be a subtype of struct type '{parent_name}'")})
                }
            }
        }
        _ => return Err(TypeError { message: format!("Child type '{}' is not a subtype of parent type '{}'", child_type.name(), parent_type.name()) })
    }

    Ok(())
}

fn methods_of_type<'a>(type_: &'a GenericType, type_environment: &'a TypeEnvironment, type_infos: &'a TypeInfos) -> Result<Vec<MethodSpecification<'a>>, TypeError> {
    match type_ {
        GenericType::TypeParameter(type_parameter) => {
            return match type_environment.get(type_parameter) {
                None => {
                    Err(TypeError { message: format!("Type parameter '{type_parameter}' is not declared in this context") })
                }
                Some(type_bound) => {
                    methods_of_type(type_bound, type_environment, type_infos)
                }
            }
        }
        GenericType::NamedType(type_name, instantiation) => {
            match type_infos.get(type_name) {
                None => {
                    Err(TypeError { message: format!("Type '{type_name}' is not declared")})
                }
                Some(type_info) => {
                    match type_info {
                        TypeInfo::Struct { bound, methods, .. } => {
                            let substitution = generate_substitution_with_bound_check(bound, instantiation, type_environment, type_infos)?;
                            let mut substituted_method_specifications = Vec::new();

                            for method_declaration in methods.values() {
                                let substituted_method = substitute_method_specification(&method_declaration.specification, &substitution)?;
                                substituted_method_specifications.push(substituted_method);
                            }

                            Ok(substituted_method_specifications)
                        }
                        TypeInfo::Interface { bound, methods } => {
                            let substitution = generate_substitution(bound, instantiation)?;
                            let mut substituted_method_specifications = Vec::new();

                            for method_specification in methods.iter() {
                                let substituted_method_specification = substitute_method_specification(method_specification, &substitution)?;
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