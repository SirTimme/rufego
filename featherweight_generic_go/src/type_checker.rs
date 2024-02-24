use std::collections::{HashMap};
use diagnostics::{MethodImplementationError, report_duplicate_method_spec_parameter, report_duplicate_type_declaration, report_invalid_method_receiver, report_unknown_type, report_duplicate_literal_parameter, TypeDeclarationError, report_duplicate_method_parameter, CheckEnvironment, report_invalid_variable, report_invalid_method_call_arg_count_mismatch, report_invalid_method_call_not_implemented, report_invalid_method_call_number, report_invalid_struct_literal, StructLiteralError, report_invalid_select_unknown_field, report_invalid_select_interface, report_invalid_bin_op, BinOpError, report_invalid_subtype_method, report_invalid_subtype_base, report_invalid_subtype_method_call, report_method_not_implemented, report_invalid_subtype_number, SubTypeNumberError, report_invalid_subtype_return_type_mismatch, report_invalid_subtype_parameter_arg_mismatch, report_invalid_subtype_parameter_type_mismatch, report_invalid_type_bound_arg_mismatch, report_duplicate_type_formal, report_invalid_subtype_struct_literal, report_invalid_type_parameter, report_unknown_type_parameter, report_invalid_assert_type_interface};
use parser::{Declaration, Expression, GenericBinding, GenericReceiver, GenericType, MethodDeclaration, MethodSpecification, Program, TypeLiteral};

// TODO Self recursion in struct
// TODO Präsi (Wann, Inhalt, Ablauf, Gespräch danach?)
// TODO Distinct Check Parameter name und Type parameter?
// TODO subtyping 2 Interfaces
// TODO subtyping mit 2 type parameters?
// TODO type parameter return value method

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
                return Err(TypeError { message: report_duplicate_type_declaration(type_name) });
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
                    let error_message = report_invalid_method_receiver(
                        method.specification.name,
                        method.receiver.type_,
                        MethodImplementationError::InterfaceReceiverType,
                    );
                    return Err(TypeError { message: error_message });
                }
                Some(TypeInfo::Struct { methods, .. }) => {
                    // is the method already declared?
                    if methods.insert(method.specification.name, method).is_some() {
                        let error_message = report_invalid_method_receiver(
                            method.specification.name,
                            method.receiver.type_,
                            MethodImplementationError::DuplicateMethodImplementation,
                        );
                        return Err(TypeError { message: error_message });
                    }
                }
                None => {
                    let error_message = report_invalid_method_receiver(
                        method.specification.name,
                        method.receiver.type_,
                        MethodImplementationError::UnknownReceiverType,
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
    expression_well_formed(&program.expression, &HashMap::new(), &HashMap::new(), type_infos, "main")
}

fn declaration_well_formed(declaration: &Declaration, type_infos: &TypeInfos) -> Result<(), TypeError> {
    match declaration {
        // method declaration well-formed?
        Declaration::Method(MethodDeclaration { receiver, specification, body }) => {
            method_well_formed(receiver, specification, body, type_infos)?
        }
        // type declaration well-formed?
        Declaration::Type { name, bound, literal } => {
            let mut psi = HashMap::new();

            // build environment for type formals of literal
            for binding in bound.iter() {
                psi.insert(binding.name, binding.type_.clone());
            }

            let _ = formal_type_well_formed(&HashMap::new(), &psi, type_infos, name, &CheckEnvironment::Literal)?;

            // type literal well-formed?
            type_literal_well_formed(name, literal, &psi, type_infos)?
        }
    }

    Ok(())
}

fn type_literal_well_formed(literal_name: &str, literal: &TypeLiteral, literal_environment: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), TypeError> {
    match literal {
        TypeLiteral::Struct { fields } => struct_well_formed(literal_name, fields, literal_environment, type_infos)?,
        TypeLiteral::Interface { methods } => interface_well_formed(literal_name, methods, literal_environment, type_infos)?,
    }

    Ok(())
}

fn struct_well_formed(struct_name: &str, fields: &[GenericBinding], type_environment: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), TypeError> {
    for (index, field) in fields.iter().enumerate() {
        // field names distinct?
        if fields.iter().skip(index + 1).any(|binding| binding.name == field.name) {
            let error_msg = report_duplicate_literal_parameter(field.name, struct_name, TypeDeclarationError::DuplicateFieldStruct);
            return Err(TypeError { message: error_msg });
        }

        // field type well-formed in the literal environment?
        type_well_formed(&field.type_, type_environment, type_infos, struct_name, &CheckEnvironment::Literal)?;
    }

    Ok(())
}

fn interface_well_formed(interface_name: &str, methods: &[MethodSpecification], type_environment: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), TypeError> {
    for (index, method_specification) in methods.iter().enumerate() {
        // name of method specification unique?
        if methods.iter().skip(index + 1).any(|method_spec| method_spec.name == method_specification.name) {
            let error_msg = report_duplicate_literal_parameter(method_specification.name, interface_name, TypeDeclarationError::DuplicateMethodInterface);
            return Err(TypeError { message: error_msg });
        }

        // method specification well-formed in the literal environment?
        method_specification_well_formed(interface_name, method_specification, type_environment, type_infos)?;
    }

    Ok(())
}

fn method_specification_well_formed(
    interface_name: &str,
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
    let delta = nested_type_formals_well_formed(literal_environment, &method_environment, type_infos, interface_name, &CheckEnvironment::MethodSpec)?;

    for (index, parameter) in specification.parameters.iter().enumerate() {
        // name of method parameter distinct?
        if specification.parameters.iter().skip(index + 1).any(|element| element.name == parameter.name) {
            return Err(TypeError { message: report_duplicate_method_spec_parameter(specification.name, interface_name, parameter.name) });
        }

        // parameter type well-formed in the concatenated environment?
        type_well_formed(&parameter.type_, &delta, type_infos, interface_name, &CheckEnvironment::MethodSpec)?;
    }

    // return type well-formed in the concatenated environment?
    type_well_formed(&specification.return_type, &delta, type_infos, interface_name, &CheckEnvironment::MethodSpec)?;

    Ok(())
}

// TODO generic return type of interface --> how to implement by struct?

fn method_well_formed(receiver: &GenericReceiver, specification: &MethodSpecification, body: &Expression, type_infos: &TypeInfos) -> Result<(), TypeError> {
    for (index, parameter) in specification.parameters.iter().enumerate() {
        // name of receiver type and parameter distinct?
        if receiver.name == parameter.name || specification.parameters.iter().skip(index + 1).any(|binding| binding.name == parameter.name) {
            return Err(TypeError { message: report_duplicate_method_parameter(receiver.type_, specification.name, parameter.name) });
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
            let delta = nested_type_formals_well_formed(&receiver_environment, &method_environment, type_infos, specification.name, &CheckEnvironment::Method)?;

            // parameter types well-formed in the concatenated environment?
            for parameter in &specification.parameters {
                type_well_formed(&parameter.type_, &delta, type_infos, specification.name, &CheckEnvironment::Method)?;
            }

            // return-type well-formed in the concatenated environment??
            type_well_formed(&specification.return_type, &delta, type_infos, specification.name, &CheckEnvironment::Method)?;

            // create environment for method parameters
            let mut variable_environment = HashMap::new();

            variable_environment.insert(receiver.name, receiver_type);

            for parameter in &specification.parameters {
                variable_environment.insert(parameter.name, parameter.type_.clone());
            }

            // body expression well-formed in the concatenated environment?
            let expression_type = expression_well_formed(body, &variable_environment, &delta, type_infos, specification.name)?;

            // body expression subtype of return type in the concatenated environment?
            match is_subtype_of(&expression_type, &specification.return_type, &delta, type_infos) {
                Ok(_) => Ok(()),
                Err(error) => {
                    let outer_error = report_invalid_subtype_method(
                        expression_type.name(),
                        specification.return_type.name(),
                        specification.name,
                        receiver.type_,
                    );

                    let error_msg = format!("{} {}", outer_error, error.message);
                    Err(TypeError { message: error_msg })
                }
            }
        }
        None => Err(TypeError { message: report_invalid_method_receiver(specification.name, receiver.type_, MethodImplementationError::UnknownReceiverType) })
    }
}

fn type_well_formed(type_: &GenericType, delta: &TypeEnvironment, type_infos: &TypeInfos, surrounding_type: &str, check_environment: &CheckEnvironment) -> Result<(), TypeError> {
    match type_ {
        GenericType::TypeParameter(type_parameter) => {
            type_parameter_well_formed(type_parameter, delta, surrounding_type, check_environment)?
        }
        GenericType::NamedType(type_name, instantiation) => {
            type_actual_well_formed(instantiation, delta, type_infos, surrounding_type, check_environment)?;

            // instantiated types satisfy type bounds of type formals?
            match type_infos.get(type_name) {
                None => return Err(TypeError { message: report_unknown_type(surrounding_type, type_name, check_environment) }),
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

fn type_parameter_well_formed(type_parameter: &str, type_environment: &TypeEnvironment, surrounding_type: &str, check_environment: &CheckEnvironment) -> Result<(), TypeError> {
    match type_environment.get(type_parameter) {
        None => Err(TypeError { message: report_unknown_type_parameter(surrounding_type, type_parameter, check_environment) }),
        Some(_) => Ok(())
    }
}

fn type_actual_well_formed(
    instantiation: &Vec<GenericType>,
    type_environment: &TypeEnvironment,
    type_infos: &TypeInfos,
    surrounding_type: &str,
    check_environment: &CheckEnvironment,
) -> Result<(), TypeError> {
    // instantiation types well-formed?
    for instantiated_type in instantiation {
        type_well_formed(instantiated_type, type_environment, type_infos, surrounding_type, check_environment)?;
    }

    Ok(())
}

fn formal_type_well_formed<'a>(
    outer: &TypeEnvironment<'a>,
    inner: &TypeEnvironment<'a>,
    type_infos: &TypeInfos,
    surrounding_type: &str,
    check_environment: &CheckEnvironment,
) -> Result<TypeEnvironment<'a>, TypeError> {
    let mut concat_environment = HashMap::new();

    // insert types of outer environment to concatenated type environment
    for (name, type_) in outer.iter() {
        concat_environment.insert(*name, type_.clone());
    }

    for (name, type_) in inner.iter() {
        // duplicate type parameter names?
        match concat_environment.insert(*name, type_.clone()) {
            Some(_) => return Err(TypeError { message: String::from("Duplicate type parameter") }),
            None => continue,
        }
    }

    // types of the inner environment well-formed in the concatenated environment?
    for type_ in inner.values() {
        type_well_formed(type_, &concat_environment, type_infos, surrounding_type, check_environment)?;
    }

    Ok(concat_environment)
}

fn nested_type_formals_well_formed<'a>(
    outer: &TypeEnvironment<'a>,
    inner: &TypeEnvironment<'a>,
    type_infos: &TypeInfos,
    surrounding_type: &str,
    check_environment: &CheckEnvironment,
) -> Result<TypeEnvironment<'a>, TypeError> {
    let _ = formal_type_well_formed(&HashMap::new(), outer, type_infos, surrounding_type, check_environment)?;
    let delta = formal_type_well_formed(outer, inner, type_infos, surrounding_type, check_environment)?;

    Ok(delta)
}

pub(crate) fn expression_well_formed<'a>(
    expression: &'a Expression<'a>,
    variable_environment: &VariableEnvironment<'a>,
    delta: &TypeEnvironment<'a>,
    type_infos: &TypeInfos<'a>,
    method_name: &str,
) -> Result<GenericType<'a>, TypeError> {
    match expression {
        Expression::Variable { name: variable_name } => {
            // variable declared in environment?
            match variable_environment.get(variable_name) {
                Some(var_type) => Ok(var_type.clone()),
                None => Err(TypeError { message: report_invalid_variable(method_name, variable_name) })
            }
        }
        Expression::MethodCall { expression, method, instantiation, parameter_expressions } => {
            let expression_type = expression_well_formed(expression, variable_environment, delta, type_infos, method_name)?;

            // TODO 2 Times substitution?            
            match expression_type {
                GenericType::TypeParameter(_) => {
                    todo!()
                }
                GenericType::NamedType(type_name, type_instantiation) => {
                    match type_infos.get(type_name).unwrap() {
                        TypeInfo::Struct { bound, methods, .. } => {
                            match methods.get(method) {
                                None => Err(TypeError { message: report_invalid_method_call_not_implemented(method_name, method, type_name) }),
                                Some(declaration) => {
                                    // TODO verify again...
                                    let method_spec_substitution = generate_substitution_with_bound_check(bound, &type_instantiation, delta, type_infos)?;
                                    let substituted_method_specification = substitute_method_specification(&declaration.specification, &method_spec_substitution)?;

                                    let method_parameters_substitution = generate_substitution_with_bound_check(&declaration.specification.bound, instantiation, delta, type_infos)?;

                                    for (index, parameter_expression) in parameter_expressions.iter().enumerate() {
                                        let parameter_expression_type = expression_well_formed(parameter_expression, variable_environment, delta, type_infos, method_name)?;
                                        let substituted_parameter_expression = substitute_type_parameter(&parameter_expression_type, &method_parameters_substitution)?;

                                        let substituted_method_spec_parameter = &substituted_method_specification.parameters.get(index).unwrap().type_;

                                        is_subtype_of(&substituted_parameter_expression, substituted_method_spec_parameter, delta, type_infos)?;
                                    }

                                    Ok(substitute_type_parameter(&substituted_method_specification.return_type, &method_parameters_substitution)?)
                                }
                            }
                        }
                        TypeInfo::Interface { bound, methods } => {
                            match methods.iter().find(|method_specification| &method_specification.name == method) {
                                None => Err(TypeError { message: report_invalid_method_call_not_implemented(method_name, method, type_name) }),
                                Some(specification) => {
                                    let substitution_map = generate_substitution(bound, instantiation)?;
                                    let substituted_method_specification = substitute_method_specification(specification, &substitution_map)?;

                                    println!("{:#?}", substituted_method_specification);

                                    todo!()
                                }
                            }
                        }
                    }
                }
                GenericType::NumberType => Err(TypeError { message: report_invalid_method_call_number(method_name) })
            }

            //     }
            //     TypeInfo::Interface { methods, .. } => {
            //         match methods.iter().find(|method_specification| &method_specification.name == method) {
            //             Some(declaration) => {
            //                 let substituted_environment = substitute_type_formals(method, &declaration.bound, instantiation, type_infos)?;
            //
            //                 // correct amount of parameters supplied?
            //                 if declaration.parameters.len() != parameter_expressions.len() {
            //                     let error_msg = report_invalid_method_call_arg_count_mismatch(
            //                         method_name,
            //                         expression_type.name(),
            //                         declaration.name,
            //                         declaration.parameters.len(),
            //                         parameter_expressions.len(),
            //                     );
            //
            //                     return Err(TypeError { message: error_msg });
            //                 }
            //
            //                 for (index, expression) in parameter_expressions.iter().enumerate() {
            //                     let expression_type = expression_well_formed(expression, variable_environment, &substituted_environment, type_infos, method_name)?;
            //                     let parameter = declaration.parameters.get(index).unwrap();
            //
            //                     variable_environment.insert(parameter.name, expression_type);
            //                 }
            //
            //                 // does the types of the parameter expressions match the types of the method parameters?
            //                 for parameter in declaration.parameters.iter() {
            //                     let parameter_type = variable_environment.get(parameter.name).unwrap();
            //
            //                     // is the supplied parameter expression subtype of the corresponding method parameter?
            //                     match is_subtype_of(parameter_type, &parameter.type_, &substituted_environment, type_infos) {
            //                         Ok(_) => (),
            //                         Err(error) => {
            //                             let base_error = report_invalid_subtype_base(parameter_type.name(), parameter_type.name());
            //                             let error_msg = format!("{} {}", base_error, error.message);
            //
            //                             return Err(TypeError { message: error_msg });
            //                         }
            //                     }
            //                 }
            //
            //                 Ok(declaration.return_type.clone())
            //             }
            //             None => {
            //                 Err(TypeError { message: report_invalid_method_call_not_implemented(method_name, method, expression_type.name()) })
            //             }
            //         }
            //     }
            // }
        }
        Expression::StructLiteral { name, instantiation, field_expressions } => {
            let struct_type = GenericType::NamedType(name, instantiation.clone());

            // struct type well-formed?
            type_well_formed(&struct_type, delta, type_infos, method_name, &CheckEnvironment::Method)?;

            match type_infos.get(name) {
                Some(type_info) => {
                    match type_info {
                        TypeInfo::Struct { bound, fields, .. } => {
                            if fields.len() != field_expressions.len() {
                                return Err(TypeError { message: format!("Struct type '{name}' has '{}' fields but '{}' were provided", fields.len(), field_expressions.len()) });
                            }

                            let mut field_expression_types = Vec::new();

                            // evaluate field expressions
                            for expression in field_expressions.iter() {
                                let field_expression_type = expression_well_formed(expression, variable_environment, delta, type_infos, method_name)?;
                                field_expression_types.push(field_expression_type);
                            }

                            // generate substitution map for struct fields
                            let substitution = generate_substitution(bound, instantiation)?;

                            // substitute fields with type parameters with their instantiation
                            let substituted_struct_fields = substitute_struct_fields(&substitution, fields)?;

                            for (index, substituted_field) in substituted_struct_fields.iter().enumerate() {
                                let expression_type = field_expression_types.get(index).unwrap();

                                is_subtype_of(expression_type, &substituted_field.type_, delta, type_infos)?;
                            }

                            Ok(struct_type)
                        }
                        TypeInfo::Interface { .. } => Err(TypeError { message: report_invalid_struct_literal(method_name, name, StructLiteralError::InterfaceType) }),
                    }
                }
                None => Err(TypeError { message: report_invalid_struct_literal(method_name, name, StructLiteralError::UnknownType) }),
            }
        }
        Expression::Select { expression, field: field_var } => {
            let expression_type = expression_well_formed(expression, variable_environment, delta, type_infos, method_name)?;

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

                            Err(TypeError { message: report_invalid_select_unknown_field(method_name, type_name, field_var) })
                        }
                        TypeInfo::Interface { .. } => Err(TypeError { message: report_invalid_select_interface(method_name, type_name) }),
                    }
                }
                _ => Err(TypeError { message: String::from("Select-expression evaluated to a number type or type parameter which is forbidden") }),
            }
        }
        Expression::TypeAssertion { expression, assert } => {
            // TODO variable as assert type?

            // asserted type well-formed?
            type_well_formed(assert, delta, type_infos, method_name, &CheckEnvironment::Method)?;

            // evaluate body expression
            let expression_type = expression_well_formed(expression, variable_environment, delta, type_infos, method_name)?;

            match expression_type {
                GenericType::TypeParameter(type_parameter) => {
                    let type_info = typeinfo_of_interface_like_type(type_parameter, type_infos, delta)?;

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
            let lhs_type = expression_well_formed(lhs, variable_environment, delta, type_infos, method_name)?;

            // right side of operation number type?
            let rhs_type = expression_well_formed(rhs, variable_environment, delta, type_infos, method_name)?;

            match (lhs_type, rhs_type) {
                (GenericType::NumberType, GenericType::NumberType) => Ok(GenericType::NumberType),
                _ => Err(TypeError { message: String::from("Either LFS or RHS of a binary operation doesn't evaluate to a number type") })
            }
        }
    }
}

fn bounds_of_type<'a>(type_: &'a GenericType, type_infos: &'a TypeInfos, delta: &'a TypeEnvironment) -> Result<&'a GenericType<'a>, TypeError> {
    match type_ {
        GenericType::TypeParameter(type_parameter) => {
            match delta.get(type_parameter) {
                None => Err(TypeError { message: format!("Type parameter '{type_parameter}' is unknown in this context") }),
                Some(type_bound) => Ok(type_bound)
            }
        }
        GenericType::NamedType(name, _) => {
            match type_infos.get(name) {
                None => Err(TypeError { message: format!("Type '{name}' is unknown in this context") }),
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
        let bound = substitute_type_parameter(bound, &generated_substitution)?;

        is_subtype_of(actual_type, &bound, type_environment, type_infos)?;
    }

    Ok(generated_substitution)
}

fn substitute_type_parameter<'a, 'b>(type_: &'a GenericType<'b>, substitution: &'a SubstitutionMap<'b>) -> Result<GenericType<'b>, TypeError> {
    match type_ {
        GenericType::TypeParameter(type_name) => {
            match substitution.get(type_name) {
                None => Ok(GenericType::TypeParameter(type_name)),
                Some(entry) => Ok(entry.clone()),
            }
        }
        GenericType::NamedType(type_name, instantiation) => {
            let mut substituted_instantiation = Vec::new();

            for instantiated_type in instantiation {
                let substituted_type = substitute_type_parameter(instantiated_type, substitution)?;
                substituted_instantiation.push(substituted_type);
            }

            Ok(GenericType::NamedType(type_name, substituted_instantiation))
        }
        GenericType::NumberType => Ok(GenericType::NumberType),
    }
}

pub(crate) fn substitute_struct_fields<'a, 'b>(substitution: &'a SubstitutionMap<'b>, fields: &'a [GenericBinding<'b>]) -> Result<Vec<GenericBinding<'b>>, TypeError> {
    let mut substituted_fields = Vec::new();

    for field_binding in fields.iter() {
        let substituted_field_type = substitute_type_parameter(&field_binding.type_, substitution)?;
        substituted_fields.push(GenericBinding { name: field_binding.name, type_: substituted_field_type });
    }

    Ok(substituted_fields)
}

fn substitute_method_specification<'a, 'b>(method_specification: &'a MethodSpecification<'b>, substitution: &'a SubstitutionMap<'b>) -> Result<MethodSpecification<'b>, TypeError> {
    let mut substituted_bound = Vec::new();

    for binding in &method_specification.bound {
        let substituted_bound_parameter = substitute_type_parameter(&binding.type_, substitution)?;
        substituted_bound.push(GenericBinding { name: binding.name, type_: substituted_bound_parameter });
    }

    let mut substituted_method_parameters = Vec::new();

    for method_parameter in &method_specification.parameters {
        let substituted_method_parameter = substitute_type_parameter(&method_parameter.type_, substitution)?;
        substituted_method_parameters.push(GenericBinding { name: method_parameter.name, type_: substituted_method_parameter })
    }

    let substituted_return_type = substitute_type_parameter(&method_specification.return_type, substitution)?;

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
            let child_type = type_environment.get(child_name).unwrap();
            let parent_type = type_environment.get(parent_name).unwrap();

            return is_subtype_of(child_type, parent_type, type_environment, type_infos);
        }
        (GenericType::NamedType(child_name, _), GenericType::NamedType(parent_name, _)) => {
            let child_type_info = type_infos.get(child_name).unwrap();
            let parent_type_info = type_infos.get(parent_name).unwrap();

            match (child_type_info, parent_type_info) {
                (TypeInfo::Struct { .. }, TypeInfo::Struct { .. }) => {
                    if child_name != parent_name {
                        return Err(TypeError { message: report_invalid_subtype_struct_literal(child_name, parent_name) });
                    }
                }
                (TypeInfo::Interface { .. }, TypeInfo::Struct { .. }) => return Err(TypeError { message: report_invalid_subtype_base(child_name, parent_name) }),
                (TypeInfo::Interface { methods: child_methods, .. }, TypeInfo::Interface { methods: parent_methods, .. }) => {
                    for parent_method in parent_methods.iter() {
                        match child_methods.iter().find(|method_spec| method_spec.name == parent_method.name) {
                            None => {
                                return Err(TypeError { message: format!("Interface type {child_name} is not a subtype of interface type {parent_name}: Missing implementation of method") });
                            }
                            Some(_) => continue,
                        }
                    }

                    return Ok(());
                }
                (TypeInfo::Struct { .. }, TypeInfo::Interface { methods, .. }) => {
                    for method in methods.iter() {
                        let method_spec = match child_type_info {
                            TypeInfo::Struct { methods, .. } => methods.get(method.name).map(|method_decl| &method_decl.specification),
                            TypeInfo::Interface { methods, .. } => methods.iter().find(|method_spec| method_spec.name == method.name),
                        };

                        match method_spec {
                            None => {
                                return Err(TypeError { message: report_method_not_implemented(method.name, child_name, parent_name) });
                            }
                            Some(method_spec) => {
                                // TODO subtype appropriate?
                                // is_subtype_of(&method.return_type, &method_spec.return_type, type_environment, type_infos)?;
                                if method.return_type != method_spec.return_type {
                                    let error_msg = report_invalid_subtype_return_type_mismatch(
                                        method_spec.name,
                                        child_name,
                                        method.return_type.name(),
                                        parent_name,
                                        method_spec.return_type.name(),
                                    );
                                    return Err(TypeError { message: error_msg });
                                }

                                if method.parameters.len() != method_spec.parameters.len() {
                                    let error_msg = report_invalid_subtype_parameter_arg_mismatch(
                                        method_spec.name,
                                        child_name,
                                        method.parameters.len(),
                                        parent_name,
                                        method_spec.parameters.len(),
                                    );
                                    return Err(TypeError { message: error_msg });
                                }

                                for (index, method_parameter) in method.parameters.iter().enumerate() {
                                    let child_method_parameter = method_spec.parameters.get(index).unwrap();

                                    if child_method_parameter.type_ != method_parameter.type_ {
                                        let error_msg = report_invalid_subtype_parameter_type_mismatch(
                                            child_method_parameter.name,
                                            method_spec.name,
                                            parent_name,
                                            method_parameter.type_.name(),
                                            child_name,
                                            child_method_parameter.type_.name(),
                                        );
                                        return Err(TypeError { message: error_msg });
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        _ => return Err(TypeError { message: report_invalid_subtype_base(child_type.name(), parent_type.name()) })
    }

    Ok(())
}