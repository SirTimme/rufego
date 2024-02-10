use std::collections::{HashMap};
use diagnostics::{MethodImplementationError, report_duplicate_method_spec_parameter, report_duplicate_type_declaration, report_invalid_method_receiver, report_unknown_type, TypeBoundError, report_invalid_receiver_type_bound, report_invalid_literal_type_bound, report_invalid_method_spec_type_bound, report_invalid_method_type_bound, report_duplicate_literal_parameter, TypeDeclarationError, report_duplicate_method_parameter, CheckEnvironment, report_invalid_type_parameter, report_invalid_variable, report_invalid_method_call_arg_count_mismatch, report_invalid_method_call_not_implemented, report_invalid_method_call_number, report_invalid_struct_literal_arg_count_mismatch, report_invalid_struct_literal, StructLiteralError, report_invalid_select_unknown_field, report_invalid_select_interface, report_invalid_assert_type_mismatch, report_invalid_bin_op, BinOpError, report_invalid_subtype_method, report_invalid_subtype_base, report_invalid_subtype_method_call, report_method_not_implemented, report_invalid_subtype_number, SubTypeNumberError, report_invalid_subtype_struct, report_invalid_subtype_return_type_mismatch, report_invalid_subtype_parameter_arg_mismatch, report_invalid_subtype_parameter_type_mismatch, report_invalid_type_bound_arg_mismatch, report_invalid_type_bound_method_arg_mismatch, report_duplicate_type_formal};
use parser::{Declaration, Expression, GenericBinding, GenericReceiver, GenericType, MethodDeclaration, MethodSpecification, Program, TypeLiteral};

// TODO Self recursion in struct
// TODO Präsi (Wann, Inhalt, Ablauf, Gespräch danach?)
// TODO Distinct Check Parameter name und Type parameter?

// TODO subtyping 2 Interfaces
// TODO subtyping mit 2 type parameters?

// Type name -> Type Info
pub(crate) type TypeInfos<'a> = HashMap<&'a str, TypeInfo<'a>>;

// Type parameter -> Bound
type TypeEnvironment<'a> = HashMap<&'a str, GenericType<'a>>;

// Variables -> Type
type VariableEnvironment<'a> = HashMap<&'a str, GenericType<'a>>;

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
pub(crate) fn check_program<'a>(program: &Program<'a>, type_infos: &TypeInfos<'a>) -> Result<GenericType<'a>, TypeError> {
    // declarations well-formed?
    for declaration in &program.declarations {
        check_declaration(declaration, type_infos)?;
    }

    // body expression well-formed in the empty type environment and empty environment?
    check_expression(&program.expression, &mut HashMap::new(), &mut HashMap::new(), type_infos, "main")
}

/*
    Checks if declaration D is well-formed

        - type declaration is well-formed
        - method declaration is well-formed
 */
fn check_declaration(declaration: &Declaration, type_infos: &TypeInfos) -> Result<(), TypeError> {
    match declaration {
        Declaration::Type { name, bound, literal } => check_type_literal(name, bound, literal, type_infos)?,
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
fn check_type_literal(literal_name: &str, bound: &[GenericBinding], literal: &TypeLiteral, type_infos: &TypeInfos) -> Result<(), TypeError> {
    // create environment for the type formals
    let mut type_environment = HashMap::new();

    // type formals well-formed in the empty type environment?
    for binding in bound {
        match type_infos.get(binding.type_.name()) {
            Some(type_info) => {
                match type_info {
                    TypeInfo::Struct { .. } => {
                        let error_msg = report_invalid_literal_type_bound(literal_name, binding.type_.name(), binding.name, TypeBoundError::StructType);
                        return Err(TypeError { message: error_msg });
                    }
                    TypeInfo::Interface { .. } => _ = type_environment.insert(binding.name, binding.type_.clone()),
                }
            }
            None => return Err(TypeError { message: report_invalid_literal_type_bound(literal_name, binding.type_.name(), binding.name, TypeBoundError::UnknownType) })
        }
    }

    match literal {
        TypeLiteral::Struct { fields } => {
            for (index, field) in fields.iter().enumerate() {
                // field names distinct?
                if fields.iter().skip(index + 1).any(|binding| binding.name == field.name) {
                    let error_msg = report_duplicate_literal_parameter(field.name, literal_name, TypeDeclarationError::DuplicateFieldStruct);
                    return Err(TypeError { message: error_msg });
                }

                // field type well-formed in the literal environment?
                check_type(&field.type_, &type_environment, type_infos, literal_name, &CheckEnvironment::Struct)?;
            }
        }
        TypeLiteral::Interface { methods } => {
            for (index, method_specification) in methods.iter().enumerate() {
                // method names unique?
                if methods.iter().skip(index + 1).any(|method_spec| method_spec.name == method_specification.name) {
                    let error_msg = report_duplicate_literal_parameter(method_specification.name, literal_name, TypeDeclarationError::DuplicateMethodInterface);
                    return Err(TypeError { message: error_msg });
                }

                // method specification well-formed in the literal environment?
                check_method_specification(literal_name, method_specification, &type_environment, type_infos)?;
            }
        }
    }

    Ok(())
}

/*
    Checks if method specification S is well-formed

        - all method parameter names are distinct
        - all method parameter types are well-formed in the concatenated environment
        - return type is well-formed in the concatenated environment
 */
fn check_method_specification(
    interface_name: &str,
    specification: &MethodSpecification,
    literal_environment: &TypeEnvironment,
    type_infos: &TypeInfos,
) -> Result<(), TypeError> {
    // create environment for method type formals
    let mut method_environment = HashMap::new();

    for binding in &specification.bound {
        // type formals of the method well-formed in the type literal environment?
        check_type(&binding.type_, literal_environment, type_infos, specification.name, &CheckEnvironment::MethodSpec)?;

        // only interface types can be a type bound
        match type_infos.get(binding.type_.name()) {
            Some(type_info) => {
                match type_info {
                    TypeInfo::Struct { .. } => {
                        let error_msg = report_invalid_method_spec_type_bound(
                            specification.name,
                            interface_name,
                            binding.type_.name(),
                            binding.name,
                            TypeBoundError::StructType,
                        );
                        return Err(TypeError { message: error_msg });
                    }
                    TypeInfo::Interface { .. } => _ = method_environment.insert(binding.name, binding.type_.clone()),
                }
            }
            None => {
                let error_msg = report_invalid_method_spec_type_bound(
                    specification.name,
                    interface_name,
                    binding.type_.name(),
                    binding.name,
                    TypeBoundError::UnknownType,
                );
                return Err(TypeError { message: error_msg });
            }
        }
    }

    let type_environment = concat_type_environments(literal_environment, &method_environment, type_infos, interface_name, CheckEnvironment::MethodSpec)?;

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
                match type_infos.get(binding.type_.name()) {
                    Some(type_info) => {
                        match type_info {
                            TypeInfo::Struct { .. } => {
                                let error_msg = report_invalid_receiver_type_bound(
                                    specification.name,
                                    receiver.type_,
                                    binding.type_.name(),
                                    binding.name,
                                    TypeBoundError::StructType,
                                );
                                return Err(TypeError { message: error_msg });
                            }
                            TypeInfo::Interface { .. } => _ = receiver_environment.insert(binding.name, binding.type_.clone()),
                        }
                    }
                    None => {
                        let error_msg = report_invalid_receiver_type_bound(
                            specification.name,
                            receiver.type_,
                            binding.type_.name(),
                            binding.name,
                            TypeBoundError::UnknownType,
                        );
                        return Err(TypeError { message: error_msg });
                    }
                }

                instantiated_types.push(binding.type_.clone());
            }

            let receiver_type = GenericType::NamedType(receiver.type_, instantiated_types);

            // collect method type formals
            let mut method_environment = HashMap::new();

            for binding in &specification.bound {
                match type_infos.get(binding.type_.name()) {
                    Some(type_info) => {
                        match type_info {
                            TypeInfo::Struct { .. } => {
                                let error_msg = report_invalid_method_type_bound(
                                    specification.name,
                                    receiver.type_,
                                    binding.type_.name(),
                                    binding.name,
                                    TypeBoundError::StructType,
                                );
                                return Err(TypeError { message: error_msg });
                            }
                            TypeInfo::Interface { .. } => _ = method_environment.insert(binding.name, binding.type_.clone()),
                        }
                    }
                    None => {
                        let error_msg = report_invalid_method_type_bound(
                            specification.name,
                            receiver.type_,
                            binding.type_.name(),
                            binding.name,
                            TypeBoundError::UnknownType,
                        );
                        return Err(TypeError { message: error_msg });
                    }
                }
            }

            let mut type_environment = concat_type_environments(&receiver_environment, &method_environment, type_infos, specification.name, CheckEnvironment::Method)?;

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
            let expression_type = check_expression(body, &mut variable_environment, &mut type_environment, type_infos, specification.name)?;

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
            if !type_environment.contains_key(type_parameter) {
                return Err(TypeError { message: report_invalid_type_parameter(surrounding_type, type_parameter, check_environment) });
            }
        }
        GenericType::NamedType(type_name, instantiation) => {
            if !type_infos.contains_key(type_name) {
                return Err(TypeError { message: report_unknown_type(surrounding_type, type_name, check_environment) });
            }

            for parameter in instantiation {
                // parameter well-formed in its environment?
                check_type(parameter, type_environment, type_infos, surrounding_type, check_environment)?;
            }

            // type bound satisfied?
            check_type_bound(type_name, instantiation, type_environment, type_infos, surrounding_type, check_environment)?;
        }
        GenericType::NumberType => (),
    }

    Ok(())
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
fn check_expression<'a>(
    expression: &Expression<'a>,
    variable_environment: &mut VariableEnvironment<'a>,
    type_environment: &mut TypeEnvironment<'a>,
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
                            check_method_bound(method, &declaration.specification.bound, instantiation, type_environment, type_infos, method_name, CheckEnvironment::Method)?;

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
                                let expression_type = check_expression(expression, variable_environment, type_environment, type_infos, method_name)?;
                                let parameter = declaration.specification.parameters.get(index).unwrap();

                                variable_environment.insert(parameter.name, expression_type);
                            }

                            // substitute type parameter with concrete type
                            for (index, binding) in declaration.specification.bound.iter().enumerate() {
                                let actual_type = instantiation.get(index).unwrap();

                                // instantiated type well-formed in its environment?
                                check_type(actual_type, type_environment, type_infos, method_name, &CheckEnvironment::Method)?;

                                type_environment.insert(binding.name, actual_type.clone());
                            }

                            // does the types of the parameter expressions match the types of the method parameters?
                            for parameter in declaration.specification.parameters.iter() {
                                let parameter_type = variable_environment.get(parameter.name).unwrap();

                                // is the supplied parameter expression subtype of the corresponding method parameter?
                                match is_subtype_of(parameter_type, &parameter.type_, type_environment, type_infos) {
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
                            check_method_bound(method, &declaration.bound, instantiation, type_environment, type_infos, method_name, CheckEnvironment::Method)?;

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
                                let expression_type = check_expression(expression, variable_environment, type_environment, type_infos, method_name)?;
                                let parameter = declaration.parameters.get(index).unwrap();

                                variable_environment.insert(parameter.name, expression_type);
                            }

                            // substitute type parameter with concrete type
                            for (index, binding) in declaration.bound.iter().enumerate() {
                                let actual_type = instantiation.get(index).unwrap();

                                // instantiated type well-formed in its environment?
                                check_type(actual_type, type_environment, type_infos, method_name, &CheckEnvironment::Method)?;

                                type_environment.insert(binding.name, actual_type.clone());
                            }

                            // does the types of the parameter expressions match the types of the method parameters?
                            for parameter in declaration.parameters.iter() {
                                let parameter_type = variable_environment.get(parameter.name).unwrap();

                                // is the supplied parameter expression subtype of the corresponding method parameter?
                                is_subtype_of(parameter_type, &parameter.type_, type_environment, type_infos)?;
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
                            // substitute generic parameters with actual types
                            for (index, binding) in bound.iter().enumerate() {
                                let actual_type = instantiation.get(index).unwrap();

                                type_environment.insert(binding.name, actual_type.clone());
                            }

                            // correct amount of parameters supplied?
                            if fields.len() != field_expressions.len() {
                                return Err(TypeError { message: report_invalid_struct_literal_arg_count_mismatch(method_name, name, fields.len(), field_expressions.len()) });
                            }

                            for (index, expression) in field_expressions.iter().enumerate() {
                                if let Some(field) = fields.get(index) {
                                    // evaluate type of supplied parameter
                                    let field_type = check_expression(expression, variable_environment, type_environment, type_infos, method_name)?;

                                    // field expression subtype of field type?
                                    is_subtype_of(&field_type, &field.type_, type_environment, type_infos)?;
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
            // asserted type well-formed?
            check_type(assert, type_environment, type_infos, method_name, &CheckEnvironment::Method)?;

            // evaluate body expression
            let expression_type = check_expression(expression, variable_environment, type_environment, type_infos, method_name)?;

            match (&expression_type, assert) {
                (GenericType::NumberType, GenericType::NumberType) => return Ok(GenericType::NumberType),
                (GenericType::NumberType, _) => return Err(TypeError { message: report_invalid_assert_type_mismatch(method_name, expression_type.name(), assert.name()) }),
                (_, GenericType::NumberType) => return Err(TypeError { message: report_invalid_assert_type_mismatch(method_name, expression_type.name(), assert.name()) }),
                (GenericType::NamedType(expression_type_name, _), GenericType::NamedType(assert_type_name, _)) => {
                    let expression_type_info = type_infos.get(expression_type_name).unwrap();
                    let assert_type_info = type_infos.get(assert_type_name).unwrap();

                    match (expression_type_info, assert_type_info) {
                        (TypeInfo::Interface { .. }, TypeInfo::Interface { .. }) => (),
                        (TypeInfo::Struct { .. }, TypeInfo::Struct { .. }) => (),
                        (TypeInfo::Interface { .. }, TypeInfo::Struct { .. }) => {
                            let error_msg = report_invalid_assert_type_mismatch(method_name, expression_type.name(), assert.name());
                            return Err(TypeError { message: error_msg });
                        }
                        (TypeInfo::Struct { .. }, TypeInfo::Interface { .. }) => is_subtype_of(assert, &expression_type, type_environment, type_infos)?
                    }
                }
                (_, GenericType::TypeParameter(assert_type_name)) => {
                    let expression_type_info = type_infos.get(expression_type.name()).unwrap();

                    let assert_type = type_environment.get(assert_type_name).unwrap();
                    let assert_type_info = type_infos.get(assert_type.name()).unwrap();

                    match (expression_type_info, assert_type_info) {
                        (TypeInfo::Interface { .. }, TypeInfo::Interface { .. }) => (),
                        (TypeInfo::Struct { .. }, TypeInfo::Struct { .. }) => (),
                        (TypeInfo::Interface { .. }, TypeInfo::Struct { .. }) => {
                            let error_msg = report_invalid_assert_type_mismatch(method_name, expression_type.name(), assert.name());
                            return Err(TypeError { message: error_msg });
                        }
                        (TypeInfo::Struct { .. }, TypeInfo::Interface { .. }) => is_subtype_of(assert, &expression_type, type_environment, type_infos)?
                    }
                }
                (_, _) => {}
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
                },
                (GenericType::NamedType(name, _), GenericType::NumberType) => {
                    Err(TypeError { message: report_invalid_bin_op(method_name, name, GenericType::NumberType.name(), BinOpError::Lhs) })
                },
                // RHS wrong
                (GenericType::NumberType, GenericType::TypeParameter(type_parameter)) => {
                    Err(TypeError { message: report_invalid_bin_op(method_name, GenericType::NumberType.name(), type_parameter, BinOpError::Rhs) })
                },
                (GenericType::NumberType, GenericType::NamedType(name, _)) => {
                    Err(TypeError { message: report_invalid_bin_op(method_name, GenericType::NumberType.name(), name, BinOpError::Rhs) })
                },
                // BOTH wrong
                (GenericType::TypeParameter(lhs_type_parameter), GenericType::TypeParameter(rhs_type_parameter)) => {
                    Err(TypeError { message: report_invalid_bin_op(method_name, lhs_type_parameter, rhs_type_parameter, BinOpError::Both) })
                },
                (GenericType::NamedType(lhs_name, _), GenericType::NamedType(rhs_name, _)) => {
                    Err(TypeError { message: report_invalid_bin_op(method_name, lhs_name, rhs_name, BinOpError::Both) })
                },
                (GenericType::TypeParameter(lhs_type), GenericType::NamedType(rhs_type, _)) => {
                    Err(TypeError { message: report_invalid_bin_op(method_name, lhs_type, rhs_type, BinOpError::Both) })
                },
                (GenericType::NamedType(lhs_type, _), GenericType::TypeParameter(rhs_type)) => {
                    Err(TypeError { message: report_invalid_bin_op(method_name, lhs_type, rhs_type, BinOpError::Both) })
                },
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
                        return Err(TypeError { message: report_invalid_subtype_struct(parent_name) });
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
                (TypeInfo::Interface { .. }, TypeInfo::Struct { .. }) => return Err(TypeError { message: report_invalid_subtype_base() }),
                (TypeInfo::Interface { .. }, TypeInfo::Interface { .. }) => return Err(TypeError { message: report_invalid_subtype_base() }),
            }
        }
        (GenericType::TypeParameter(child_name), GenericType::TypeParameter(parent_name)) => {
            if child_name != parent_name {
                return Err(TypeError { message: report_invalid_subtype_base() });
            }
        }
        (_, GenericType::TypeParameter(parent_name)) => {
            let parent_type = type_environment.get(parent_name).unwrap();
            return is_subtype_of(child_type, parent_type, type_environment, type_infos);
        }
        (GenericType::TypeParameter(_), GenericType::NamedType(_, _)) => return Err(TypeError { message: report_invalid_subtype_base() }),
    }

    Ok(())
}

fn concat_type_environments<'a>(
    outer: &TypeEnvironment<'a>,
    inner: &TypeEnvironment<'a>,
    type_infos: &TypeInfos,
    surrounding_type: &str,
    check_environment: CheckEnvironment
) -> Result<TypeEnvironment<'a>, TypeError> {
    let mut concat_environment = HashMap::new();

    // types in the outer environment well-formed in the empty environment?
    for (name, type_) in outer {
        check_type(type_, &HashMap::new(), type_infos, surrounding_type, &check_environment)?;

        concat_environment.insert(*name, type_.clone());
    }

    // types in the inner environment well-formed in the outer environment?
    for (name, type_) in inner {
        check_type(type_, outer, type_infos, surrounding_type, &check_environment)?;

        match concat_environment.insert(*name, type_.clone()) {
            Some(_) => return Err(TypeError { message: report_duplicate_type_formal(surrounding_type, name) }),
            None => continue,
        }
    }

    Ok(concat_environment)
}

/*
    Checks if type bound is satisfied

        - correct amount of parameters supplied
        - instantiated types well-formed in its environment
        - instantiated type subtype of type bound
 */
fn check_type_bound(
    type_: &str,
    instantiation: &Vec<GenericType>,
    type_environment: &TypeEnvironment,
    types: &TypeInfos,
    surrounding_type: &str,
    check_environment: &CheckEnvironment
) -> Result<(), TypeError> {
    match types.get(type_).unwrap() {
        TypeInfo::Struct { bound, .. } => {
            // correct amount of parameters supplied?
            if bound.len() != instantiation.len() {
                return Err(TypeError { message: report_invalid_type_bound_arg_mismatch(type_, bound.len(), instantiation.len()) });
            }

            // bound = formal type
            // instantiation = actual types
            for (index, parameter) in bound.iter().enumerate() {
                let actual_type = instantiation.get(index).unwrap();

                // actual type well-formed in its environment?
                check_type(actual_type, type_environment, types, surrounding_type, check_environment)?;

                // instantiated type subtype of type bound?
                is_subtype_of(actual_type, &parameter.type_, type_environment, types)?;
            }
        }
        TypeInfo::Interface { bound, .. } => {
            // correct amount of parameters supplied?
            if bound.len() != instantiation.len() {
                return Err(TypeError { message: report_invalid_type_bound_arg_mismatch(type_, bound.len(), instantiation.len()) });
            }

            for (index, parameter) in bound.iter().enumerate() {
                let instantiated_type = instantiation.get(index).unwrap();

                // instantiated type well-formed in its environment?
                check_type(instantiated_type, type_environment, types, surrounding_type, check_environment)?;

                // instantiated type subtype of type bound?
                is_subtype_of(instantiated_type, &parameter.type_, type_environment, types)?;
            }
        }
    }

    Ok(())
}

fn check_method_bound(
    method_name: &str,
    formal_bound: &Vec<GenericBinding>,
    instantiation: &Vec<GenericType>,
    type_environment: &TypeEnvironment,
    types: &TypeInfos,
    surrounding_type: &str,
    check_environment: CheckEnvironment
) -> Result<(), TypeError> {
    // correct amount of parameters supplied?
    if formal_bound.len() != instantiation.len() {
        return Err(TypeError { message: report_invalid_type_bound_method_arg_mismatch(method_name, formal_bound.len(), instantiation.len()) });
    }

    for (index, parameter) in formal_bound.iter().enumerate() {
        let actual_type = instantiation.get(index).unwrap();

        // instantiated type well-formed in its environment?
        check_type(actual_type, type_environment, types, surrounding_type, &check_environment)?;

        // instantiated type subtype of type bound?
        is_subtype_of(actual_type, &parameter.type_, type_environment, types)?;
    }

    Ok(())
}