use std::collections::{HashMap};
use diagnostics::{MethodImplementationError, report_duplicate_method_spec_parameter, report_duplicate_type_declaration, report_invalid_method_receiver, report_unknown_type, report_duplicate_literal_parameter, TypeDeclarationError, report_duplicate_method_parameter, CheckEnvironment, report_invalid_variable, report_invalid_method_call_arg_count_mismatch, report_invalid_method_call_not_implemented, report_invalid_method_call_number, report_invalid_struct_literal, StructLiteralError, report_invalid_select_unknown_field, report_invalid_select_interface, report_invalid_bin_op, BinOpError, report_invalid_subtype_method, report_invalid_subtype_base, report_invalid_subtype_method_call, report_method_not_implemented, report_invalid_subtype_number, SubTypeNumberError, report_invalid_subtype_return_type_mismatch, report_invalid_subtype_parameter_arg_mismatch, report_invalid_subtype_parameter_type_mismatch, report_invalid_type_bound_arg_mismatch, report_duplicate_type_formal, report_invalid_subtype_struct_literal, report_invalid_type_parameter, report_unknown_type_parameter, report_invalid_assert_type_interface};
use parser::{Declaration, Expression, GenericBinding, GenericReceiver, GenericType, MethodDeclaration, MethodSpecification, Program, TypeLiteral};

// TODO Self recursion in struct
// TODO Präsi (Wann, Inhalt, Ablauf, Gespräch danach?)
// TODO Distinct Check Parameter name und Type parameter?
// TODO substitution not only in method call?
// TODO subtyping 2 Interfaces
// TODO subtyping mit 2 type parameters?

// Type name -> Type Info
pub(crate) type TypeInfos<'a> = HashMap<&'a str, TypeInfo<'a>>;

// Type parameter -> Bound
pub(crate) type TypeEnvironment<'a> = HashMap<&'a str, GenericType<'a>>;

// Variables -> Type
pub(crate) type VariableEnvironment<'a> = HashMap<&'a str, GenericType<'a>>;

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

impl<'a> TypeInfo<'a> {
    fn method_specification(&self, method_name: &'a str) -> Option<&'a MethodSpecification<'a>> {
        match self {
            TypeInfo::Struct { methods, .. } => methods.get(method_name).map(|method| &method.specification),
            TypeInfo::Interface { methods, .. } => methods.iter().find(|method| method.name == method_name),
        }
    }
}

#[derive(Debug)]
pub(crate) struct TypeError {
    pub(crate) message: String,
}

/*
    Creates info for types

        Struct:
            - type formals (bound)
            - fields
            - implemented methods for that type
        Interface:
            - type formals (bound)
            - method specifications
 */
pub(crate) fn create_type_infos<'a>(program: &'a Program<'a>) -> Result<TypeInfos, TypeError> {
    let mut type_infos = HashMap::new();

    // collect type metadata for type declarations
    for declaration in &program.declarations {
        if let Declaration::Type { name: type_name, bound, literal } = declaration {
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

    // collect metadata for method declarations
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

/*
    Checks if program P is well-formed

        - all type declarations are distinct
        - all method declarations are distinct
        - all declarations are well-formed
        - body expression well-formed in the empty context
 */
pub(crate) fn check_program<'a>(program: &'a Program<'a>, type_infos: &TypeInfos<'a>) -> Result<GenericType<'a>, TypeError> {
    // declarations well-formed?
    for declaration in &program.declarations {
        check_declaration(declaration, type_infos)?;
    }

    // body expression well-formed in the empty type environment and empty environment?
    check_expression(&program.expression, &mut HashMap::new(), &HashMap::new(), type_infos, "main")
}

/*
    Checks if declaration D is well-formed

        - type declaration is well-formed
        - method declaration is well-formed
 */
fn check_declaration(declaration: &Declaration, type_infos: &TypeInfos) -> Result<(), TypeError> {
    match declaration {
        Declaration::Type { name, bound, literal } => {
            let mut literal_environment = HashMap::new();

            for binding in bound.iter() {
                check_type(&binding.type_, &literal_environment, type_infos, name, &CheckEnvironment::Literal)?;

                literal_environment.insert(binding.name, binding.type_.clone());
            }

            check_type_literal(name, literal, &literal_environment, type_infos)?
        }
        Declaration::Method(MethodDeclaration { receiver, specification, body }) => check_method(receiver, specification, body, type_infos)?,
    }

    Ok(())
}

/*
    Checks if type literal T is well-formed

        - type formals well-formed in the empty type environment
        - literal well-formed in the type formals environment

        Struct:
            - all field names are distinct
            - all field types well-formed in the literal environment
        Interface:
            - all its method specifications are well-formed in the literal environment
            - all method names are unique
 */
fn check_type_literal(literal_name: &str, literal: &TypeLiteral, literal_environment: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), TypeError> {
    match literal {
        TypeLiteral::Struct { fields } => check_struct(literal_name, fields, literal_environment, type_infos)?,
        TypeLiteral::Interface { methods } => check_interface(literal_name, methods, literal_environment, type_infos)?,
    }

    Ok(())
}

fn check_struct(struct_name: &str, fields: &[GenericBinding], type_environment: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), TypeError> {
    for (index, field) in fields.iter().enumerate() {
        // field names distinct?
        if fields.iter().skip(index + 1).any(|binding| binding.name == field.name) {
            let error_msg = report_duplicate_literal_parameter(field.name, struct_name, TypeDeclarationError::DuplicateFieldStruct);
            return Err(TypeError { message: error_msg });
        }

        // field type well-formed in the literal environment?
        check_type(&field.type_, type_environment, type_infos, struct_name, &CheckEnvironment::Literal)?;
    }

    Ok(())
}

fn check_interface(interface_name: &str, methods: &[MethodSpecification], type_environment: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), TypeError> {
    for (index, method_specification) in methods.iter().enumerate() {
        // method names unique?
        if methods.iter().skip(index + 1).any(|method_spec| method_spec.name == method_specification.name) {
            let error_msg = report_duplicate_literal_parameter(method_specification.name, interface_name, TypeDeclarationError::DuplicateMethodInterface);
            return Err(TypeError { message: error_msg });
        }

        // method specification well-formed in the literal environment?
        check_method_specification(interface_name, method_specification, type_environment, type_infos)?;
    }

    Ok(())
}

/*
    Checks if method specification S is well-formed

        - all method parameter names are distinct
        - all method parameter types are well-formed in the concatenated environment
        - return type is well-formed in the concatenated environment
 */
fn check_method_specification(interface_name: &str, specification: &MethodSpecification, literal_environment: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), TypeError> {
    // create environment for method type formals
    let mut method_environment = HashMap::new();

    for binding in &specification.bound {
        method_environment.insert(binding.name, binding.type_.clone());
    }

    let type_environment = check_nested_type_formals(literal_environment, &method_environment, type_infos, interface_name, &CheckEnvironment::MethodSpec)?;

    for (index, parameter) in specification.parameters.iter().enumerate() {
        // parameter names distinct?
        if specification.parameters.iter().skip(index + 1).any(|element| element.name == parameter.name) {
            return Err(TypeError { message: report_duplicate_method_spec_parameter(specification.name, interface_name, parameter.name) });
        }

        // parameter type well-formed in the concatenated environment?
        check_type(&parameter.type_, &type_environment, type_infos, interface_name, &CheckEnvironment::MethodSpec)?;
    }

    // return type well-formed in the concatenated environment?
    check_type(&specification.return_type, &type_environment, type_infos, interface_name, &CheckEnvironment::MethodSpec)?;

    Ok(())
}

fn check_nested_type_formals<'a>(
    outer_environment: &TypeEnvironment<'a>,
    inner_environment: &TypeEnvironment<'a>,
    type_infos: &TypeInfos,
    surrounding_type: &str,
    check_environment: &CheckEnvironment,
) -> Result<TypeEnvironment<'a>, TypeError> {
    let mut concat_environment = HashMap::new();

    // types in the outer environment well-formed in the empty environment?
    for (name, type_) in outer_environment.iter() {
        check_type(type_, &HashMap::new(), type_infos, surrounding_type, check_environment)?;

        concat_environment.insert(*name, type_.clone());
    }

    // types in the inner environment well-formed in the outer environment?
    for (name, type_) in inner_environment.iter() {
        check_type(type_, outer_environment, type_infos, surrounding_type, check_environment)?;

        match concat_environment.insert(*name, type_.clone()) {
            Some(_) => return Err(TypeError { message: report_duplicate_type_formal(surrounding_type, name) }),
            None => continue,
        }
    }

    Ok(concat_environment)
}

/*
    Checks if a method declaration is well-formed

        - receiver and method parameter names are distinct
        - receiver type well-formed
        - type formals of the receiver type subtype of the type declaration
        - method parameter types well-formed in the concatenated environment
        - return type well-formed in the concatenated environment
        - body expression well-formed in the concatenated environment
        - body expression type is subtype of the return type
 */
fn check_method(receiver: &GenericReceiver, specification: &MethodSpecification, body: &Expression, type_infos: &TypeInfos) -> Result<(), TypeError> {
    for (index, parameter) in specification.parameters.iter().enumerate() {
        // receiver name and parameter names distinct?
        if receiver.name == parameter.name || specification.parameters.iter().skip(index + 1).any(|binding| binding.name == parameter.name) {
            return Err(TypeError { message: report_duplicate_method_parameter(receiver.type_, specification.name, parameter.name) });
        }
    }

    // receiver type declared?
    match type_infos.get(receiver.type_) {
        Some(_) => {
            // collect receiver type formals
            let mut receiver_environment = HashMap::new();

            // keep track of bound types
            let mut instantiated_types = Vec::new();

            for binding in &receiver.instantiation {
                receiver_environment.insert(binding.name, binding.type_.clone());

                instantiated_types.push(binding.type_.clone());
            }

            let receiver_type = GenericType::NamedType(receiver.type_, instantiated_types);

            // collect method type formals
            let mut method_environment = HashMap::new();

            for binding in &specification.bound {
                method_environment.insert(binding.name, binding.type_.clone());
            }

            let type_environment = check_nested_type_formals(&receiver_environment, &method_environment, type_infos, specification.name, &CheckEnvironment::Method)?;

            // parameter types well-formed in the concatenated environment?
            for parameter in &specification.parameters {
                check_type(&parameter.type_, &type_environment, type_infos, specification.name, &CheckEnvironment::Method)?;
            }

            // return-type well-formed in the concatenated environment??
            check_type(&specification.return_type, &type_environment, type_infos, specification.name, &CheckEnvironment::Method)?;

            // create an environment for the variables
            let mut variable_environment = HashMap::new();

            variable_environment.insert(receiver.name, receiver_type);

            for parameter in &specification.parameters {
                variable_environment.insert(parameter.name, parameter.type_.clone());
            }

            // body expression well-formed in the concatenated environment?
            let expression_type = check_expression(body, &mut variable_environment, &type_environment, type_infos, specification.name)?;

            // type of body expression subtype of return type in the concatenated environment?
            match is_subtype_of(&expression_type, &specification.return_type, &type_environment, type_infos) {
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

/*
    Checks if type t is well-formed

        - all type parameters must be declared in its environment
        - all named types must be instantiated with type arguments that satisfy its bounds
*/
fn check_type(
    type_: &GenericType,
    type_environment: &TypeEnvironment,
    type_infos: &TypeInfos,
    surrounding_type: &str,
    check_environment: &CheckEnvironment,
) -> Result<(), TypeError> {
    match type_ {
        GenericType::TypeParameter(type_parameter) => {
            // type parameter declared?
            return match type_environment.get(type_parameter) {
                // type parameter is not declared in current environment
                None => Err(TypeError { message: report_unknown_type_parameter(surrounding_type, type_parameter, check_environment) }),
                Some(type_) => {
                    match type_infos.get(type_.name()) {
                        // type parameter is not declared in current environment
                        None => Err(TypeError { message: report_unknown_type(surrounding_type, type_.name(), check_environment) }),
                        Some(type_info) => {
                            match type_info {
                                // type parameter is a struct type
                                TypeInfo::Struct { .. } => Err(TypeError { message: report_invalid_type_parameter(surrounding_type, type_parameter, type_.name()) }),
                                TypeInfo::Interface { .. } => Ok(())
                            }
                        }
                    }
                }
            };
        }
        GenericType::NamedType(type_name, instantiation) => {
            // type actual well-formed?
            for type_ in instantiation {
                check_type(type_, type_environment, type_infos, surrounding_type, check_environment)?;
            }

            // type declared?
            match type_infos.get(type_name) {
                None => return Err(TypeError { message: report_unknown_type(surrounding_type, type_name, check_environment) }),
                Some(type_info) => {
                    match type_info {
                        TypeInfo::Struct { bound, .. } => _ = substitute_type_formals(type_name, bound, instantiation, type_infos)?,
                        TypeInfo::Interface { bound, .. } => _ = substitute_type_formals(type_name, bound, instantiation, type_infos)?,
                    }
                }
            }
        }
        GenericType::NumberType => (),
    }

    Ok(())
}

fn substitute_type_formals<'a>(
    type_name: &str,
    type_formal: &'a Vec<GenericBinding>,
    instantiation: &'a Vec<GenericType>,
    type_infos: &TypeInfos,
) -> Result<TypeEnvironment<'a>, TypeError> {
    let mut substituted_environment = HashMap::new();

    // correct amount of type parameters supplied?
    if type_formal.len() != instantiation.len() {
        return Err(TypeError { message: report_invalid_type_bound_arg_mismatch(type_name, type_formal.len(), instantiation.len()) });
    }

    for (index, formal_type) in type_formal.iter().enumerate() {
        let actual_type = instantiation.get(index).unwrap();

        // actual type subtype of formal type?
        is_subtype_of(actual_type, &formal_type.type_, &substituted_environment, type_infos)?;

        // substitute type formals with type actuals and check bound
        substituted_environment.insert(formal_type.name, actual_type.clone());
    }

    Ok(substituted_environment)
}

/*
    Checks if expression e is well-formed

        Variable:
            - variable known in the environment
        Method call:
            - method is implemented for type of body expression
            - body expression and parameter expressions well-formed
            - parameter expression subtype from method parameter type
        Struct literal:
            - struct type well-formed in its environment
            - all field values provided
            - each field expression is a subtype of its field type
        Select:
            - expression is a struct type
            - selected field is part of the expression type
        Type assertion:
            - asserted type well-formed in its environment
            - expression subtype of asserted type
        Number:
            - no prerequisites
        BinOp:
            - left side of operation is a number type
            - right side of operation is a number type
 */
pub(crate) fn check_expression<'a>(
    expression: &'a Expression<'a>,
    variable_environment: &mut VariableEnvironment<'a>,
    type_environment: &TypeEnvironment<'a>,
    type_infos: &TypeInfos<'a>,
    method_name: &str,
) -> Result<GenericType<'a>, TypeError> {
    match expression {
        Expression::Variable { name: variable_name } => {
            // variable known?
            match variable_environment.get(variable_name) {
                Some(var_type) => Ok(var_type.clone()),
                None => Err(TypeError { message: report_invalid_variable(method_name, variable_name) })
            }
        }
        Expression::MethodCall { expression, method, instantiation, parameter_expressions } => {
            let expression_type = check_expression(expression, variable_environment, type_environment, type_infos, method_name)?;

            let type_info = match expression_type {
                GenericType::TypeParameter(type_parameter) => {
                    let actual_type = type_environment.get(type_parameter).unwrap();
                    type_infos.get(actual_type.name()).unwrap()
                }
                GenericType::NamedType(type_name, _) => type_infos.get(type_name).unwrap(),
                GenericType::NumberType => return Err(TypeError { message: report_invalid_method_call_number(method_name) })
            };

            match type_info {
                TypeInfo::Struct { methods, .. } => {
                    match methods.get(method) {
                        Some(declaration) => {
                            let substituted_environment = substitute_type_formals(method, &declaration.specification.bound, instantiation, type_infos)?;

                            // correct amount of parameters supplied?
                            if declaration.specification.parameters.len() != parameter_expressions.len() {
                                let error_msg = report_invalid_method_call_arg_count_mismatch(
                                    method_name,
                                    expression_type.name(),
                                    declaration.specification.name,
                                    declaration.specification.parameters.len(),
                                    parameter_expressions.len(),
                                );

                                return Err(TypeError { message: error_msg });
                            }

                            for (index, expression) in parameter_expressions.iter().enumerate() {
                                let expression_type = check_expression(expression, variable_environment, &substituted_environment, type_infos, method_name)?;
                                let parameter = declaration.specification.parameters.get(index).unwrap();

                                variable_environment.insert(parameter.name, expression_type);
                            }

                            // does the types of the parameter expressions match the types of the method parameters?
                            for parameter in declaration.specification.parameters.iter() {
                                let parameter_type = variable_environment.get(parameter.name).unwrap();

                                // is the supplied parameter expression subtype of the corresponding method parameter?
                                match is_subtype_of(parameter_type, &parameter.type_, &substituted_environment, type_infos) {
                                    Ok(_) => (),
                                    Err(error) => {
                                        let outer_error = report_invalid_subtype_method_call(method, parameter.name, parameter.type_.name(), parameter_type.name());
                                        let error_msg = format!("{} {}", error.message, outer_error);

                                        return Err(TypeError { message: error_msg });
                                    }
                                }
                            }

                            Ok(declaration.specification.return_type.clone())
                        }
                        None => {
                            Err(TypeError { message: report_invalid_method_call_not_implemented(method_name, method, expression_type.name()) })
                        }
                    }
                }
                TypeInfo::Interface { methods, .. } => {
                    match methods.iter().find(|method_specification| &method_specification.name == method) {
                        Some(declaration) => {
                            let substituted_environment = substitute_type_formals(method, &declaration.bound, instantiation, type_infos)?;

                            // correct amount of parameters supplied?
                            if declaration.parameters.len() != parameter_expressions.len() {
                                let error_msg = report_invalid_method_call_arg_count_mismatch(
                                    method_name,
                                    expression_type.name(),
                                    declaration.name,
                                    declaration.parameters.len(),
                                    parameter_expressions.len(),
                                );

                                return Err(TypeError { message: error_msg });
                            }

                            for (index, expression) in parameter_expressions.iter().enumerate() {
                                let expression_type = check_expression(expression, variable_environment, &substituted_environment, type_infos, method_name)?;
                                let parameter = declaration.parameters.get(index).unwrap();

                                variable_environment.insert(parameter.name, expression_type);
                            }

                            // does the types of the parameter expressions match the types of the method parameters?
                            for parameter in declaration.parameters.iter() {
                                let parameter_type = variable_environment.get(parameter.name).unwrap();

                                // is the supplied parameter expression subtype of the corresponding method parameter?
                                match is_subtype_of(parameter_type, &parameter.type_, &substituted_environment, type_infos) {
                                    Ok(_) => (),
                                    Err(error) => {
                                        let base_error = report_invalid_subtype_base(parameter_type.name(), parameter_type.name());
                                        let error_msg = format!("{} {}", base_error, error.message);

                                        return Err(TypeError { message: error_msg });
                                    }
                                }
                            }

                            Ok(declaration.return_type.clone())
                        }
                        None => {
                            Err(TypeError { message: report_invalid_method_call_not_implemented(method_name, method, expression_type.name()) })
                        }
                    }
                }
            }
        }
        Expression::StructLiteral { name, instantiation, field_expressions } => {
            let struct_type = GenericType::NamedType(name, instantiation.clone());

            // struct type well-formed?
            check_type(&struct_type, type_environment, type_infos, method_name, &CheckEnvironment::Method)?;

            match type_infos.get(name) {
                Some(type_info) => {
                    match type_info {
                        TypeInfo::Struct { bound, fields, .. } => {
                            // TODO order of preconditions?
                            let substituted_environment = substitute_type_formals(name, bound, instantiation, type_infos)?;

                            for (index, expression) in field_expressions.iter().enumerate() {
                                if let Some(field) = fields.get(index) {
                                    // evaluate type of supplied parameter
                                    let field_type = check_expression(expression, variable_environment, &substituted_environment, type_infos, method_name)?;

                                    // field expression subtype of field type?
                                    match is_subtype_of(&field_type, &field.type_, &substituted_environment, type_infos) {
                                        Ok(_) => (),
                                        Err(error) => {
                                            let base_error = report_invalid_subtype_base(field_type.name(), field.type_.name());
                                            let error_msg = format!("{} {}", base_error, error.message);

                                            return Err(TypeError { message: error_msg });
                                        }
                                    }
                                }
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
            let expression_type = check_expression(expression, variable_environment, type_environment, type_infos, method_name)?;

            match type_infos.get(expression_type.name()).unwrap() {
                TypeInfo::Struct { fields, .. } => {
                    // selected field part of the struct type?
                    match fields.iter().find(|field| &field.name == field_var) {
                        Some(field) => Ok(field.type_.clone()),
                        None => Err(TypeError { message: report_invalid_select_unknown_field(method_name, expression_type.name(), field_var) }),
                    }
                }
                TypeInfo::Interface { .. } => Err(TypeError { message: report_invalid_select_interface(method_name, expression_type.name()) }),
            }
        }
        Expression::TypeAssertion { expression, assert } => {
            // evaluate body expression
            let expression_type = check_expression(expression, variable_environment, type_environment, type_infos, method_name)?;

            // get type info for expression type
            let expression_type_info = match type_infos.get(expression_type.name()) {
                None => {
                    match type_environment.get(assert.name()) {
                        None => {
                            match variable_environment.get(assert.name()) {
                                None => return Err(TypeError { message: report_unknown_type(method_name, assert.name(), &CheckEnvironment::Method) }),
                                Some(var_type) => type_infos.get(var_type.name()).unwrap(),
                            }
                        }
                        Some(type_) => type_infos.get(type_.name()).unwrap(),
                    }
                }
                Some(type_info) => type_info,
            };

            let assert_type = match type_infos.get(assert.name()) {
                None => {
                    match type_environment.get(assert.name()) {
                        None => {
                            match variable_environment.get(assert.name()) {
                                None => return Err(TypeError { message: report_unknown_type(method_name, assert.name(), &CheckEnvironment::Method) }),
                                Some(var_type) => var_type,
                            }
                        }
                        Some(type_) => type_,
                    }
                }
                Some(_) => assert,
            };

            let assert_type_info = type_infos.get(assert_type.name()).unwrap();

            // asserted type well-formed?
            check_type(assert_type, type_environment, type_infos, method_name, &CheckEnvironment::Method)?;

            match (expression_type_info, assert_type_info) {
                (TypeInfo::Interface { .. }, TypeInfo::Interface { .. }) => (),
                (TypeInfo::Struct { .. }, TypeInfo::Struct { .. }) => (),
                (TypeInfo::Interface { .. }, TypeInfo::Struct { .. }) => is_subtype_of(assert_type, &expression_type, type_environment, type_infos)?,
                (TypeInfo::Struct { .. }, TypeInfo::Interface { .. }) => {
                    let error_msg = report_invalid_assert_type_interface(method_name, expression_type.name(), assert_type.name());
                    return Err(TypeError { message: error_msg });
                }
            }

            Ok(assert.clone())
        }
        Expression::Number { .. } => {
            Ok(GenericType::NumberType)
        }
        Expression::BinOp { lhs, rhs, .. } => {
            // left side of operation number type?
            let lhs_type = check_expression(lhs, variable_environment, type_environment, type_infos, method_name)?;

            // right side of operation number type?
            let rhs_type = check_expression(rhs, variable_environment, type_environment, type_infos, method_name)?;

            match (lhs_type, rhs_type) {
                // OK
                (GenericType::NumberType, GenericType::NumberType) => Ok(GenericType::NumberType),
                // LHS wrong
                (GenericType::TypeParameter(type_parameter), GenericType::NumberType) => {
                    Err(TypeError { message: report_invalid_bin_op(method_name, type_parameter, GenericType::NumberType.name(), BinOpError::Lhs) })
                }
                (GenericType::NamedType(name, _), GenericType::NumberType) => {
                    Err(TypeError { message: report_invalid_bin_op(method_name, name, GenericType::NumberType.name(), BinOpError::Lhs) })
                }
                // RHS wrong
                (GenericType::NumberType, GenericType::TypeParameter(type_parameter)) => {
                    Err(TypeError { message: report_invalid_bin_op(method_name, GenericType::NumberType.name(), type_parameter, BinOpError::Rhs) })
                }
                (GenericType::NumberType, GenericType::NamedType(name, _)) => {
                    Err(TypeError { message: report_invalid_bin_op(method_name, GenericType::NumberType.name(), name, BinOpError::Rhs) })
                }
                // BOTH wrong
                (GenericType::TypeParameter(lhs_type_parameter), GenericType::TypeParameter(rhs_type_parameter)) => {
                    Err(TypeError { message: report_invalid_bin_op(method_name, lhs_type_parameter, rhs_type_parameter, BinOpError::Both) })
                }
                (GenericType::NamedType(lhs_name, _), GenericType::NamedType(rhs_name, _)) => {
                    Err(TypeError { message: report_invalid_bin_op(method_name, lhs_name, rhs_name, BinOpError::Both) })
                }
                (GenericType::TypeParameter(lhs_type), GenericType::NamedType(rhs_type, _)) => {
                    Err(TypeError { message: report_invalid_bin_op(method_name, lhs_type, rhs_type, BinOpError::Both) })
                }
                (GenericType::NamedType(lhs_type, _), GenericType::TypeParameter(rhs_type)) => {
                    Err(TypeError { message: report_invalid_bin_op(method_name, lhs_type, rhs_type, BinOpError::Both) })
                }
            }
        }
    }
}

/*
    Checks if child_type is a subtype of parent_type
 */
pub(crate) fn is_subtype_of(child_type: &GenericType, parent_type: &GenericType, type_environment: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), TypeError> {
    // a type is a subtype of itself
    if child_type.name() == parent_type.name() {
        return Ok(());
    }

    match (child_type, parent_type) {
        (GenericType::NumberType, GenericType::NumberType) => (),
        (GenericType::NumberType, _) => return Err(TypeError { message: report_invalid_subtype_number(child_type.name(), parent_type.name(), SubTypeNumberError::Lhs) }),
        (_, GenericType::NumberType) => return Err(TypeError { message: report_invalid_subtype_number(child_type.name(), parent_type.name(), SubTypeNumberError::Rhs) }),
        (GenericType::NamedType(child_name, _), GenericType::NamedType(parent_name, _)) => {
            let child_type_info = type_infos.get(child_name).unwrap();
            let parent_type_info = type_infos.get(parent_name).unwrap();

            match (child_type_info, parent_type_info) {
                (TypeInfo::Struct { .. }, TypeInfo::Struct { .. }) => {
                    if child_name != parent_name {
                        return Err(TypeError { message: report_invalid_subtype_struct_literal(child_name, parent_name) });
                    }
                }
                (TypeInfo::Struct { .. }, TypeInfo::Interface { methods, .. }) => {
                    for method in methods.iter() {
                        match child_type_info.method_specification(method.name) {
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
                (TypeInfo::Interface { .. }, TypeInfo::Struct { .. }) => return Err(TypeError { message: report_invalid_subtype_base(child_name, parent_name) }),
                (TypeInfo::Interface { .. }, TypeInfo::Interface { .. }) => return Err(TypeError { message: report_invalid_subtype_base(child_name, parent_name) }),
            }
        }
        (GenericType::TypeParameter(child_name), GenericType::TypeParameter(parent_name)) => {
            if child_name != parent_name {
                return Err(TypeError { message: report_invalid_subtype_base(child_name, parent_name) });
            }
        }
        (_, GenericType::TypeParameter(parent_name)) => {
            let parent_type = type_environment.get(parent_name).unwrap();
            return is_subtype_of(child_type, parent_type, type_environment, type_infos);
        }
        (GenericType::TypeParameter(child_name), GenericType::NamedType(parent_name, _)) => return Err(TypeError { message: report_invalid_subtype_base(child_name, parent_name) }),
    }

    Ok(())
}