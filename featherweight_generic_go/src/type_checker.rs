use std::collections::{HashMap};
use diagnostic::{report_assert_type_mismatch, report_duplicate_field_name, report_duplicate_method_implementation, report_duplicate_method_name, report_duplicate_method_parameter, report_duplicate_type, report_duplicate_type_formal, report_interface_implementation, report_invalid_binop, report_literal_wrong_argcount, report_method_not_implemented, report_select_interface, report_struct_literal_interface, report_unknown_field, report_unknown_interface_method, report_unknown_receiver_type, report_unknown_struct_literal, report_unknown_type, report_unknown_type_formal, report_unknown_type_parameter, report_unknown_variable, report_wrong_method_parameters, report_wrong_type_bound};
use parser::{Declaration, Expression, GenericBinding, GenericReceiver, GenericType, MethodDeclaration, MethodSpecification, Program, TypeLiteral};

// TODO ok to use String here for errormessages?
// TODO Self recursion in struct
// TODO assert for int?
// TODO subtype for int?
// TODO formal/actual typing? (currently only formal)
// TODO typecontexts already in FG used
// TODO diagnostics namespace
// TODO Methodcall-Chaining environment passen
// TODO Präsi (Wann, Inhalt, Ablauf, Gespräch danach?)

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
pub(crate) fn create_type_infos<'a>(program: &'a Program<'a>) -> Result<HashMap<&'a str, TypeInfo<'a>>, TypeError> {
    let mut type_infos = HashMap::new();

    // collect type metadata for type declarations
    for declaration in &program.declarations {
        if let Declaration::Type { name: type_name, bound, literal } = declaration {
            if type_infos.contains_key(type_name) {
                return Err(TypeError { message: report_duplicate_type(type_name) });
            } else {
                let type_metadata = match literal {
                    TypeLiteral::Struct { fields } => TypeInfo::Struct { bound, fields, methods: HashMap::new() },
                    TypeLiteral::Interface { methods } => TypeInfo::Interface { bound, methods },
                };

                type_infos.insert(*type_name, type_metadata);
            }
        }
    }

    // collect metadata for method declarations
    for declaration in &program.declarations {
        if let Declaration::Method(method) = declaration {
            match type_infos.get_mut(method.receiver.type_) {
                Some(TypeInfo::Interface { .. }) => return Err(TypeError { message: report_interface_implementation(method.specification.name, method.receiver.type_) }),
                Some(TypeInfo::Struct { methods, .. }) => {
                    // is the method already declared?
                    if methods.insert(method.specification.name, method).is_some() {
                        return Err(TypeError { message: report_duplicate_method_implementation(method.specification.name, method.receiver.type_) });
                    }
                }
                None => return Err(TypeError { message: report_unknown_receiver_type(method.receiver.type_, method.specification.name) }),
            }
        }
    }

    Ok(type_infos)
}

/*
    Checks if program P is well formed

        - all type declarations are distinct
        - all method declarations are distinct
        - all declarations are well formed
        - body expression well formed in the empty context
 */
pub(crate) fn check_program<'a>(program: &'a Program<'a>, type_infos: &HashMap<&'a str, TypeInfo<'a>>) -> Result<GenericType<'a>, TypeError> {
    // declarations well formed?
    for declaration in &program.declarations {
        check_declaration(declaration, type_infos)?;
    }

    // body expression well formed in the empty context?
    check_expression(&program.expression, &HashMap::new(), type_infos)
}

/*
    Checks if declaration D is well formed

        - type declaration is well formed
        - method declaration is well formed
 */
fn check_declaration<'a>(declaration: &Declaration<'a>, type_infos: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), TypeError> {
    match declaration {
        Declaration::Type { name, bound, literal } => check_type_literal(name, bound, literal, type_infos)?,
        Declaration::Method(MethodDeclaration { receiver, specification, body }) => check_method(receiver, specification, body, type_infos)?,
    }

    Ok(())
}

/*
    Checks if type literal T is well formed under the type formals

        - type formals well formed in the empty type environment
        - literal well formed in the type formals environment

        Struct:
            - all field names are distinct
            - all field types well formed in the literal environment
        Interface:
            - all its method specifications are well formed in the literal environment
            - all method names are unique
 */
fn check_type_literal<'a>(name: &'a str, bound: &[GenericBinding<'a>], literal: &TypeLiteral, type_infos: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), TypeError> {
    // create environment for the type formals
    let mut environment = HashMap::new();

    // type formals well formed in the empty type environment?
    for binding in bound {
        match obtain_nested_typeinfo(&binding.type_, &environment, type_infos)? {
            TypeInfo::Struct { .. } => return Err(TypeError { message: report_wrong_type_bound() }),
            TypeInfo::Interface { .. } => {
                environment.insert(binding.name, binding.type_.clone());
            }
        }
    }

    match literal {
        TypeLiteral::Struct { fields } => {
            for (index, field) in fields.iter().enumerate() {
                // field names distinct?
                if fields.iter().skip(index + 1).any(|binding| binding.name == field.name) {
                    return Err(TypeError { message: report_duplicate_field_name(field.name, name) });
                }

                // field type well formed in the literal environment?
                check_type(&field.type_, &environment, type_infos)?;
            }
        }
        TypeLiteral::Interface { methods } => {
            for (index, method_specification) in methods.iter().enumerate() {
                // method specification well formed in the literal environment?
                check_method_specification(method_specification, &environment, type_infos)?;

                // method names unique?
                if methods.iter().skip(index + 1).any(|method_spec| method_spec.name == method_specification.name) {
                    return Err(TypeError { message: report_duplicate_method_name(method_specification.name, name) });
                }
            }
        }
    }

    Ok(())
}

/*
    Checks if a method declaration is well formed

        - receiver and method parameter names are distinct
        - receiver type well formed
        - type formals of the receiver type subtype of the type declaration
        - method parameter types well formed in the concatenated environment
        - return type well formed in the concatenated environment
        - body expression well formed in the concatenated environment
        - body expression type is subtype of the return type
 */
fn check_method(receiver: &GenericReceiver, specification: &MethodSpecification, body: &Expression, type_infos: &HashMap<&str, TypeInfo>) -> Result<(), TypeError> {
    // keep track of bound types
    let mut instantiated_types = Vec::new();

    // create type formal environment for method receiver
    let mut receiver_environment = HashMap::new();

    // type formals well formed in the empty type environment?
    for binding in &receiver.instantiation {
        match obtain_nested_typeinfo(&binding.type_, &receiver_environment, type_infos)? {
            TypeInfo::Struct { .. } => return Err(TypeError { message: report_wrong_type_bound() }),
            TypeInfo::Interface { .. } => {
                receiver_environment.insert(binding.name, binding.type_.clone());
            }
        }

        instantiated_types.push(binding.type_.clone());
    }

    // create receiver type
    let receiver_type = GenericType::NamedType(receiver.type_, instantiated_types);

    // receiver type well formed in the receiver type environment?
    check_type(&receiver_type, &receiver_environment, type_infos)?;

    // are the type formals of the method distinct to the type formals of the receiver?
    for binding in specification.bound.iter() {
        match receiver_environment.get(binding.name) {
            Some(_) => return Err(TypeError { message: report_duplicate_type_formal(binding.name, specification.name) }),
            None => continue,
        }
    }

    // create environment for method type formals
    let mut method_environment = HashMap::new();

    for binding in &specification.bound {
        // type formals of the method well formed in the receiver environment?
        check_type(&binding.type_, &receiver_environment, type_infos)?;

        match obtain_nested_typeinfo(&binding.type_, &receiver_environment, type_infos)? {
            TypeInfo::Struct { .. } => return Err(TypeError { message: report_wrong_type_bound() }),
            TypeInfo::Interface { .. } => {
                method_environment.insert(binding.name, binding.type_.clone());
            }
        }
    }

    // concatenate both type environments
    let mut environment = method_environment.into_iter().chain(receiver_environment).collect();

    for (index, parameter) in specification.parameters.iter().enumerate() {
        // method parameter type well formed?
        check_type(&parameter.type_, &environment, type_infos)?;

        // parameter names distinct?
        if receiver.name == parameter.name || specification.parameters.iter().skip(index + 1).any(|binding| binding.name == parameter.name) {
            return Err(TypeError { message: report_duplicate_method_parameter(parameter.name, specification.name) });
        }
    }

    // return-type well formed?
    check_type(&specification.return_type, &environment, type_infos)?;

    environment.insert(receiver.name, receiver_type);

    for parameter in &specification.parameters {
        environment.insert(parameter.name, parameter.type_.clone());
    }

    // body expression well formed in the concatenated environment?
    let expression_type = check_expression(body, &environment, type_infos)?;

    // type of body expression subtype of return type in the concatenated environment?
    is_subtype_of(&expression_type, &specification.return_type, &environment, type_infos)?;

    Ok(())
}

/*
    Checks if method specification S is well formed

        - all method parameter names are distinct
        - all method parameter types are well formed in the concatenated environment
        - return type is well formed in the concatenated environment
 */
fn check_method_specification<'a>(specification: &MethodSpecification, literal_environment: &HashMap<&'a str, GenericType<'a>>, type_infos: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), TypeError> {
    // type formals of the method distinct to the type formals of the type literal?
    for binding in specification.bound.iter() {
        match literal_environment.get(binding.name) {
            Some(_) => return Err(TypeError { message: report_duplicate_type_formal(binding.name, specification.name) }),
            None => continue,
        }
    }

    // create environment for method type formals
    let mut method_environment = HashMap::new();

    for binding in &specification.bound {
        // type formals of the method well formed in the type literal environment?
        check_type(&binding.type_, literal_environment, type_infos)?;

        // only interface types can be a type bound
        match obtain_nested_typeinfo(&binding.type_, literal_environment, type_infos)? {
            TypeInfo::Struct { .. } => return Err(TypeError { message: report_wrong_type_bound() }),
            TypeInfo::Interface { .. } => {
                method_environment.insert(binding.name, binding.type_.clone());
            }
        }
    }

    // concatenate both type environments
    let mut environment = HashMap::new();

    // insert bound of type literal
    for (key, value) in literal_environment.iter() {
        environment.insert(*key, value.clone());
    }

    // insert bound of method
    for (key, value) in method_environment.iter() {
        environment.insert(key, value.clone());
    }

    for (index, parameter) in specification.parameters.iter().enumerate() {
        // parameter type well formed in the concatenated environment?
        check_type(&parameter.type_, &environment, type_infos)?;

        // parameter names distinct?
        if specification.parameters.iter().skip(index + 1).any(|element| element.name == parameter.name) {
            return Err(TypeError { message: report_duplicate_method_parameter(parameter.name, specification.name) });
        }
    }

    // return type well formed in the concatenated environment?
    check_type(&specification.return_type, &environment, type_infos)?;

    Ok(())
}

/*
    Checks if type t is well formed

        - all type parameters must be declared in its environment
        - all named types must be instantiated with type arguments that satisfy its bounds
*/
fn check_type<'a>(type_: &GenericType<'a>, environment: &HashMap<&'a str, GenericType<'a>>, type_infos: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), TypeError> {
    match type_ {
        GenericType::TypeParameter(type_parameter) => {
            if !environment.contains_key(type_parameter) {
                return Err(TypeError { message: report_unknown_type_parameter(type_parameter) });
            }
        }
        GenericType::NamedType(name, instantiation) => {
            if !type_infos.contains_key(name) {
                return Err(TypeError { message: report_unknown_type(name) });
            }

            for parameter in instantiation {
                // parameter well formed in its environment?
                check_type(parameter, environment, type_infos)?;
            }

            // type bound satisfied?
            check_type_bound(name, instantiation, environment, type_infos)?;
        }
        GenericType::NumberType => (),
    }

    Ok(())
}

/*
    Checks if expression e is well formed

        Variable:
            - variable known in the environment
        Method call:
            - method is implemented for type of body expression
            - body expression and parameter expressions well formed
            - parameter expression subtype from method parameter type
        Struct literal:
            - struct type well formed in its environment
            - all field values provided
            - each field expression is a subtype of its field type
        Select:
            - expression is a struct type
            - selected field is part of the expression type
        Type assertion:
            - asserted type well formed in its environment
            - expression subtype of asserted type
        Number:
            - no prerequisites
        BinOp:
            - left side of operation is a number type
            - right side of operation is a number type
 */
fn check_expression<'a>(expression: &Expression<'a>, environment: &HashMap<&str, GenericType<'a>>, type_infos: &HashMap<&'a str, TypeInfo<'a>>) -> Result<GenericType<'a>, TypeError> {
    match expression {
        Expression::Variable { name } => {
            // variable known in the environment?
            match environment.get(name) {
                Some(var_type) => Ok(var_type.clone()),
                None => Err(TypeError { message: report_unknown_variable(name) })
            }
        }
        Expression::MethodCall { expression, method, parameter_expressions, .. } => {
            // evaluate type of the body expression
            let expression_type = check_expression(expression, environment, type_infos)?;

            match obtain_nested_typeinfo(&expression_type, environment, type_infos)? {
                TypeInfo::Struct { methods, .. } => {
                    // is the method implemented for this type?
                    match methods.get(method) {
                        Some(declaration) => {
                            // correct amount of parameters supplied?
                            if parameter_expressions.len() != declaration.specification.parameters.len() {
                                return Err(TypeError { message: report_wrong_method_parameters(method, declaration.specification.parameters.len(), parameter_expressions.len()) });
                            }

                            // does the types of the parameter expressions match the types of the method parameters?
                            for (index, expression) in parameter_expressions.iter().enumerate() {
                                if let Some(parameter) = declaration.specification.parameters.get(index) {
                                    // evaluate type of the supplied parameter expression
                                    let expression_type = check_expression(expression, environment, type_infos)?;

                                    // is the parameter expression a subtype of the method parameter?
                                    is_subtype_of(&expression_type, &parameter.type_, environment, type_infos)?;
                                }
                            }

                            // is the return type well formed?
                            check_type(&declaration.specification.return_type, environment, type_infos)?;

                            Ok(declaration.specification.return_type.clone())
                        }
                        None => {
                            Err(TypeError { message: report_method_not_implemented(method, expression_type.name()) })
                        }
                    }
                }
                TypeInfo::Interface { methods, .. } => {
                    // does the method exist on the interface?
                    match methods.iter().find(|method_specification| &method_specification.name == method) {
                        Some(method_specification) => {
                            // correct amount of parameters supplied?
                            if parameter_expressions.len() != method_specification.parameters.len() {
                                return Err(TypeError { message: report_wrong_method_parameters(method, method_specification.parameters.len(), parameter_expressions.len()) });
                            }

                            for (index, expression) in parameter_expressions.iter().enumerate() {
                                if let Some(parameter) = method_specification.parameters.get(index) {
                                    // evaluate type of the supplied parameter expression
                                    let expression_type = check_expression(expression, environment, type_infos)?;

                                    // is the parameter at least a subtype of the method parameter?
                                    is_subtype_of(&expression_type, &parameter.type_, environment, type_infos)?;
                                }
                            }

                            // is the return type declared?
                            check_type(&method_specification.return_type, environment, type_infos)?;

                            Ok(method_specification.return_type.clone())
                        }
                        None => {
                            Err(TypeError { message: report_unknown_interface_method(expression_type.name(), method) })
                        }
                    }
                }
            }
        }
        Expression::StructLiteral { name, instantiation, field_expressions } => {
            match type_infos.get(name) {
                Some(type_info) => {
                    match type_info {
                        TypeInfo::Struct { bound, fields, .. } => {
                            // create custom environment for struct literals
                            let mut struct_literal_environment = HashMap::new();

                            for binding in bound.iter() {
                                struct_literal_environment.insert(binding.name, binding.type_.clone());
                            }

                            for (key, value) in environment
                            {
                                struct_literal_environment.insert(key, value.clone());
                            }

                            // bound correct instantiated?
                            check_type_bound(name, instantiation, &struct_literal_environment, type_infos)?;

                            // correct amount of parameters supplied?
                            if field_expressions.len() != fields.len() {
                                return Err(TypeError { message: report_literal_wrong_argcount(name, fields.len(), field_expressions.len()) });
                            }

                            for (index, expression) in field_expressions.iter().enumerate() {
                                if let Some(field) = fields.get(index) {
                                    // evaluate type of supplied parameter
                                    let field_type = check_expression(expression, &struct_literal_environment, type_infos)?;

                                    // field expression subtype of field type?
                                    is_subtype_of(&field_type, &field.type_, &struct_literal_environment, type_infos)?;
                                }
                            }

                            Ok(GenericType::NamedType(name, instantiation.clone()))
                        }
                        TypeInfo::Interface { .. } => Err(TypeError { message: report_struct_literal_interface(name) }),
                    }
                }
                None => Err(TypeError { message: report_unknown_struct_literal(name) }),
            }
        }
        Expression::Select { expression, field: field_var } => {
            let expression_type = check_expression(expression, environment, type_infos)?;

            match obtain_nested_typeinfo(&expression_type, environment, type_infos)? {
                TypeInfo::Struct { fields, .. } => {
                    // selected field part of the struct type?
                    match fields.iter().find(|field| &field.name == field_var) {
                        Some(field) => Ok(field.type_.clone()),
                        None => Err(TypeError { message: report_unknown_field(field_var, expression_type.name()) }),
                    }
                }
                TypeInfo::Interface { .. } => Err(TypeError { message: report_select_interface() }),
            }
        }
        Expression::TypeAssertion { expression, assert } => {
            // asserted type well formed?
            check_type(assert, environment, type_infos)?;

            // evaluate body expression
            let expression_type = check_expression(expression, environment, type_infos)?;

            let body_type_metadata = match expression_type {
                GenericType::NumberType => {
                    return if assert == &GenericType::NumberType {
                        Ok(GenericType::NumberType)
                    } else {
                        Err(TypeError { message: report_assert_type_mismatch(expression_type.name(), assert.name()) })
                    };
                }
                _ => obtain_nested_typeinfo(&expression_type, environment, type_infos)?,
            };

            let assert_type_metadata = match assert {
                GenericType::NumberType => {
                    return if expression_type == GenericType::NumberType {
                        Ok(GenericType::NumberType)
                    } else {
                        Err(TypeError { message: report_assert_type_mismatch(expression_type.name(), assert.name()) })
                    };
                }
                _ => obtain_nested_typeinfo(&expression_type, environment, type_infos)?,
            };

            match (assert_type_metadata, body_type_metadata) {
                (TypeInfo::Interface { .. }, TypeInfo::Interface { .. }) => (),
                (TypeInfo::Interface { .. }, TypeInfo::Struct { .. }) => return Err(TypeError { message: report_assert_type_mismatch(expression_type.name(), assert.name()) }),
                (TypeInfo::Struct { .. }, TypeInfo::Interface { .. }) => {
                    is_subtype_of(assert, &expression_type, environment, type_infos)?;
                }
                (TypeInfo::Struct { .. }, TypeInfo::Struct { .. }) => (),
            }

            Ok(assert.clone())
        }
        Expression::Number { .. } => {
            Ok(GenericType::NumberType)
        }
        Expression::BinOp { lhs, rhs, .. } => {
            // left side of operation number type?
            let lhs_type = check_expression(lhs, environment, type_infos)?;

            // right side of operation number type?
            let rhs_type = check_expression(rhs, environment, type_infos)?;

            match (lhs_type, rhs_type) {
                (GenericType::NumberType, GenericType::NumberType) => Ok(GenericType::NumberType),
                (_, _) => Err(TypeError { message: report_invalid_binop() }),
            }
        }
    }
}

/*
    Checks if child_type is a subtype of parent_type
 */
pub(crate) fn is_subtype_of<'a>(child_type: &GenericType, parent_type: &GenericType, environment: &HashMap<&str, GenericType<'a>>, type_infos: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), TypeError> {
    // a type is a subtype of itself
    if parent_type == child_type {
        return Ok(());
    }

    if child_type == &GenericType::NumberType {
        return Err(TypeError { message: String::from("ERROR: An integer value cant be the child type of a struct value") });
    }

    let child_type_info = obtain_nested_typeinfo(child_type, environment, type_infos)?;

    let parent_type_info = obtain_nested_typeinfo(parent_type, environment, type_infos)?;

    let methods = match parent_type_info {
        TypeInfo::Struct { .. } => {
            return Err(TypeError { message: String::from("ERROR: An struct value cant be the parent type") });
        }
        TypeInfo::Interface { methods, .. } => methods,
    };

    for method in methods.iter() {
        match child_type_info.method_specification(method.name) {
            None => {
                return Err(TypeError { message: format!("ERROR: Method '{}' of parent type '{:?}' is not implemented for child type '{:?}'", method.name, parent_type, child_type) });
            }
            Some(method_spec) => {
                if method.return_type != method_spec.return_type {
                    return Err(TypeError { message: format!("ERROR: Method {:?} of parent type {:?} has return type {:?} but return type of child implementation is {:?}", method.name, parent_type, method.return_type, method_spec.return_type) });
                }

                if method.parameters.len() != method_spec.parameters.len() {
                    return Err(TypeError { message: format!("ERROR: Method {:?} of parent type {:?} has {:?} parameters but child implementation has {:?} parameters", method.name, parent_type, method.parameters.len(), method_spec.parameters.len()) });
                }

                for (index, method_parameter) in method.parameters.iter().enumerate() {
                    let child_method_parameter = method_spec.parameters.get(index).expect("Method parameter should be supplied");

                    if child_method_parameter.type_ != method_parameter.type_ {
                        return Err(TypeError { message: format!("ERROR: Method parameter {:?} of method {:?} of parent type {:?} has type {:?} but parameter type of child implementation is {:?}", method_parameter.type_, method.name, parent_type, method.return_type, child_method_parameter) });
                    }
                }
            }
        }
    }

    Ok(())
}

/*
    Gets type info for arbitrary nested types
 */
fn obtain_nested_typeinfo<'a>(type_: &GenericType, environment: &HashMap<&str, GenericType>, type_infos: &HashMap<&str, TypeInfo<'a>>) -> Result<TypeInfo<'a>, TypeError> {
    match type_infos.get(type_.name()) {
        Some(type_info) => Ok(type_info.clone()),
        None => {
            match type_ {
                GenericType::NumberType => Err(TypeError { message: report_wrong_type_bound() }),
                _ => {
                    match environment.get(type_.name()) {
                        Some(nested_type) => Ok(obtain_nested_typeinfo(nested_type, environment, type_infos)?),
                        None => Err(TypeError { message: report_unknown_type_formal(type_.name()) }),
                    }
                }
            }
        }
    }
}

/*
    Checks if type bound is satisfied

        - correct amount of parameters supplied
        - instantiated types well formed in its environment
        - instantiated type subtype of type bound
 */
fn check_type_bound<'a>(type_: &'a str, instantiation: &Vec<GenericType<'a>>, environment: &HashMap<&'a str, GenericType<'a>>, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), TypeError> {
    let type_info = types.get(type_).expect("Occurrence of type was checked beforehand");

    match type_info {
        TypeInfo::Struct { bound, .. } => {
            // correct amount of parameters supplied?
            if bound.len() != instantiation.len() {
                return Err(TypeError { message: format!("ERROR: Type '{type_}' has {} generic parameters but {} parameters were supplied", bound.len(), instantiation.len()) });
            }

            for (index, parameter) in bound.iter().enumerate() {
                let instantiated_type = instantiation.get(index).unwrap();

                // instantiated type well formed in its environment?
                check_type(instantiated_type, environment, types)?;

                // instantiated type subtype of type bound?
                is_subtype_of(instantiated_type, &parameter.type_, environment, types)?;
            }
        }
        TypeInfo::Interface { bound, .. } => {
            // correct amount of parameters supplied?
            if bound.len() != instantiation.len() {
                return Err(TypeError { message: format!("ERROR: Type '{type_}' has {} generic parameters but {} parameters were supplied", bound.len(), instantiation.len()) });
            }

            for (index, parameter) in bound.iter().enumerate() {
                let instantiated_type = instantiation.get(index).unwrap();

                // instantiated type well formed in its environment?
                check_type(instantiated_type, environment, types)?;

                // instantiated type subtype of type bound?
                is_subtype_of(instantiated_type, &parameter.type_, environment, types)?;
            }
        }
    }

    Ok(())
}