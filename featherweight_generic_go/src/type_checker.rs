use std::collections::{HashMap};
use diagnostic::{report_assert_type_mismatch, report_duplicate_field_name, report_duplicate_method_implementation, report_duplicate_method_name, report_duplicate_method_parameter, report_duplicate_type, report_duplicate_type_formal, report_interface_implementation, report_interfaces_forbidden, report_invalid_binop, report_literal_wrong_argcount, report_select_interface, report_select_number, report_select_type_parameter, report_struct_literal_interface, report_unknown_field, report_unknown_receiver_type, report_unknown_struct_literal, report_unknown_type, report_unknown_type_formal, report_unknown_type_parameter, report_unknown_variable, report_wrong_type_bound};
use parser::{Declaration, Expression, GenericBinding, GenericReceiver, GenericType, MethodDeclaration, MethodSpecification, Program, TypeLiteral};

// clone() loswerden
// TODO ok to use String here for errormessages?
// TODO Self recursion in struct
// TODO assert for int?
// TODO subtype for int?
// TODO formal/actual typing? (currently only formal)
// TODO typecontexts already in FG used
// TODO wegen Pascal fragen..
// TODO Präsi (Wann, Inhalt, Ablauf, Gespräch danach?)

#[derive(Eq, PartialEq, Debug, Clone)]
pub(crate) enum TypeMetaData<'a> {
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

impl<'a> TypeMetaData<'a> {
    fn method_specification(&self, method_name: &'a str) -> Option<&'a MethodSpecification<'a>> {
        match self {
            TypeMetaData::Struct { methods, .. } => methods.get(method_name).map(|method| &method.specification),
            TypeMetaData::Interface { methods, .. } => methods.iter().find(|method| method.name == method_name),
        }
    }
}

/*
    Creates metadata for every type

        Struct:
            - type formals (bound)
            - fields
            - implemented methods for that type
        Interface:
            - type formals (bound)
            - method specifications
 */
pub(crate) fn create_type_metadata<'a>(program: &'a Program<'a>) -> Result<HashMap<&'a str, TypeMetaData<'a>>, TypeError> {
    let mut metadata = HashMap::new();

    // collect type metadata for type declarations
    for declaration in &program.declarations {
        if let Declaration::Type { name, bound, literal } = declaration {
            if metadata.contains_key(name) {
                return Err(TypeError { message: report_duplicate_type(name) });
            } else {
                let type_metadata = match literal {
                    TypeLiteral::Struct { fields } => TypeMetaData::Struct { bound, fields, methods: HashMap::new() },
                    TypeLiteral::Interface { methods } => TypeMetaData::Interface { bound, methods },
                };

                metadata.insert(*name, type_metadata);
            }
        }
    }

    // collect metadata for method declarations
    for declaration in &program.declarations {
        if let Declaration::Method(method) = declaration {
            match metadata.get_mut(method.receiver.type_) {
                Some(TypeMetaData::Interface { .. }) => return Err(TypeError { message: report_interface_implementation(method.specification.name, method.receiver.type_) }),
                Some(TypeMetaData::Struct { methods, .. }) => {
                    // is the method already declared?
                    if methods.insert(method.specification.name, method).is_some() {
                        return Err(TypeError { message: report_duplicate_method_implementation(method.specification.name, method.receiver.type_) });
                    }
                }
                None => return Err(TypeError { message: report_unknown_receiver_type(method.receiver.type_, method.specification.name) }),
            }
        }
    }

    Ok(metadata)
}

/*
    Check if program P is well formed

        - all type declarations are distinct
        - all method declarations are distinct
        - all declarations are well formed
        - body expression well formed in the empty context
 */
pub(crate) fn check_program<'a>(program: &'a Program<'a>, types: &HashMap<&'a str, TypeMetaData<'a>>) -> Result<GenericType<'a>, TypeError> {
    // declarations well formed?
    for declaration in &program.declarations {
        check_declaration(declaration, types)?;
    }

    // body expression well formed in the empty context?
    check_expression(&program.expression, &HashMap::new(), types)
}

/*
    Check if declaration D is well formed

        - type declaration is well formed
        - method declaration is well formed
 */
fn check_declaration<'a>(declaration: &Declaration<'a>, types: &HashMap<&'a str, TypeMetaData<'a>>) -> Result<(), TypeError> {
    match declaration {
        Declaration::Type { name, bound, literal } => check_type_literal(name, bound, literal, types)?,
        Declaration::Method(MethodDeclaration { receiver, specification, body }) => check_method(receiver, specification, body, types)?,
    }

    Ok(())
}

/*
    Check if type literal T is well formed under the type formals

        - type formals well formed in the empty type environment
        - literal well formed in the type formals environment

        Struct:
            - all field names are distinct
            - all field types well formed in the literal environment
        Interface:
            - all its method specifications are well formed in the literal environment
            - all method names are unique
 */
fn check_type_literal<'a>(name: &'a str, bound: &[GenericBinding<'a>], type_literal: &TypeLiteral, types: &HashMap<&'a str, TypeMetaData<'a>>) -> Result<(), TypeError> {
    // create environment for the type formals
    let mut literal_environment = HashMap::new();

    // type formals well formed in the empty type environment?
    for binding in bound {
        match obtain_nested_typeinfo(&binding.type_, &literal_environment, types)? {
            TypeMetaData::Struct { .. } => return Err(TypeError { message: report_wrong_type_bound() }),
            TypeMetaData::Interface { .. } => {
                literal_environment.insert(binding.name, binding.type_.clone());
            }
        }
    }

    match type_literal {
        TypeLiteral::Struct { fields } => {
            for (index, field) in fields.iter().enumerate() {
                // field names distinct?
                if fields.iter().skip(index + 1).any(|binding| binding.name == field.name) {
                    return Err(TypeError { message: report_duplicate_field_name(field.name, name) });
                }

                // field type well formed in the literal environment?
                check_type(&field.type_, &literal_environment, types)?;
            }
        }
        TypeLiteral::Interface { methods } => {
            for (index, method_specification) in methods.iter().enumerate() {
                // method specification well formed in the literal environment?
                check_method_specification(method_specification, &literal_environment, types)?;

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
    Check if method declaration is well formed

        - receiver and method parameter names are distinct
        - receiver type well formed
        - type formals of the receiver type subtype of the type declaration
        - method parameter types well formed in the concatenated environment
        - return type well formed in the concatenated environment
        - body expression well formed in the concatenated environment
        - body expression type is subtype of the return type
 */
fn check_method(receiver: &GenericReceiver, specification: &MethodSpecification, body: &Expression, types: &HashMap<&str, TypeMetaData>) -> Result<(), TypeError> {
    // keep track of bound types
    let mut instantiated_types = Vec::new();

    // create type formal environment for method receiver
    let mut receiver_environment = HashMap::new();

    // type formals well formed in the empty type environment?
    for binding in &receiver.instantiation {
        match obtain_nested_typeinfo(&binding.type_, &receiver_environment, types)? {
            TypeMetaData::Struct { .. } => return Err(TypeError { message: report_wrong_type_bound() }),
            TypeMetaData::Interface { .. } => {
                receiver_environment.insert(binding.name, binding.type_.clone());
            }
        }

        instantiated_types.push(binding.type_.clone());
    }

    // create receiver type
    let receiver_type = GenericType::NamedType(receiver.type_, instantiated_types);

    // receiver type well formed in the receiver type environment?
    check_type(&receiver_type, &receiver_environment, types)?;

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
        check_type(&binding.type_, &receiver_environment, types)?;

        match obtain_nested_typeinfo(&binding.type_, &receiver_environment, types)? {
            TypeMetaData::Struct { .. } => return Err(TypeError { message: report_wrong_type_bound() }),
            TypeMetaData::Interface { .. } => {
                method_environment.insert(binding.name, binding.type_.clone());
            }
        }
    }

    // concatenate both type environments
    let mut environment = method_environment.into_iter().chain(receiver_environment).collect();

    for (index, parameter) in specification.parameters.iter().enumerate() {
        // method parameter type well formed?
        check_type(&parameter.type_, &environment, types)?;

        // parameter names distinct?
        if receiver.name == parameter.name || specification.parameters.iter().skip(index + 1).any(|binding| binding.name == parameter.name) {
            return Err(TypeError { message: report_duplicate_method_parameter(parameter.name, specification.name) });
        }
    }

    // return-type well formed?
    check_type(&specification.return_type, &environment, types)?;

    environment.insert(receiver.name, receiver_type);

    for parameter in &specification.parameters {
        environment.insert(parameter.name, parameter.type_.clone());
    }

    // body expression well formed in the concatenated environment?
    let expression_type = check_expression(body, &environment, types)?;

    // type of body expression subtype of return type in the concatenated environment?
    is_subtype_of(&expression_type, &specification.return_type, &environment, types)?;

    Ok(())
}

/*
    Check if method specification S is well formed

        - all method parameter names are distinct
        - all method parameter types are well formed in the concatenated environment
        - return type is well formed in the concatenated environment
 */
fn check_method_specification<'a>(specification: &MethodSpecification, type_literal_environment: &HashMap<&'a str, GenericType<'a>>, types: &HashMap<&'a str, TypeMetaData<'a>>) -> Result<(), TypeError> {
    // type formals of the method distinct to the type formals of the type literal?
    for binding in specification.bound.iter() {
        match type_literal_environment.get(binding.name) {
            Some(_) => return Err(TypeError { message: report_duplicate_type_formal(binding.name, specification.name) }),
            None => continue,
        }
    }

    // create environment for method type formals
    let mut method_environment = HashMap::new();

    for binding in &specification.bound {
        // type formals of the method well formed in the type literal environment?
        check_type(&binding.type_, type_literal_environment, types)?;

        let type_info = obtain_nested_typeinfo(&binding.type_, type_literal_environment, types)?;

        match type_info {
            TypeMetaData::Struct { .. } => return Err(TypeError { message: report_wrong_type_bound() }),
            TypeMetaData::Interface { .. } => {
                method_environment.insert(binding.name, binding.type_.clone());
            }
        }
    }

    // concatenate both type environments
    let mut environment = HashMap::new();

    // insert bound of type literal
    for (key, value) in type_literal_environment.iter() {
        environment.insert(*key, value.clone());
    }

    // insert bound of method
    for (key, value) in method_environment.iter() {
        environment.insert(key, value.clone());
    }

    for (index, parameter) in specification.parameters.iter().enumerate() {
        // parameter type well formed in the concatenated environment?
        check_type(&parameter.type_, &environment, types)?;

        // parameter names distinct?
        if specification.parameters.iter().skip(index + 1).any(|element| element.name == parameter.name) {
            return Err(TypeError { message: report_duplicate_method_parameter(parameter.name, specification.name) });
        }
    }

    // return type well formed in the concatenated environment?
    check_type(&specification.return_type, &environment, types)?;

    Ok(())
}

/*
    Check if type is well formed

        - all type parameters must be declared in its environment
        - all named types must be instantiated with type arguments that satisfy its bounds
*/
fn check_type<'a>(type_: &GenericType<'a>, environment: &HashMap<&'a str, GenericType<'a>>, types: &HashMap<&'a str, TypeMetaData<'a>>) -> Result<(), TypeError> {
    match type_ {
        GenericType::TypeParameter(type_parameter) => {
            if !environment.contains_key(type_parameter) {
                return Err(TypeError { message: report_unknown_type_parameter(type_parameter) });
            }
        }
        GenericType::NamedType(name, instantiation) => {
            if !types.contains_key(name) {
                return Err(TypeError { message: report_unknown_type(name) });
            }

            for parameter in instantiation {
                // parameter well formed in its environment?
                check_type(parameter, environment, types)?;
            }

            // type bound satisfied?
            check_type_bound(name, instantiation, environment, types)?;
        }
        GenericType::NumberType => (),
    }

    Ok(())
}

/*
    Check if expression e is well formed

        Variable:
            - variable known in its environment
        Methodcall:
            // TODO tbd
        Struct literal:
            - struct type well formed in its environment
            - all field values provided
            - each field expression is a subtype of its field type
        Select:
            - expression is a struct type
            - selected field is part of the expression type
        TypeAssertion:
            - asserted type well formed in its environment
            - expression subtype of asserted type
        Number:
            - no prerequisites
        BinOp:
            - left side of operation is a number type
            - right side of operation is a number type
 */
fn check_expression<'a>(expression: &Expression<'a>, environment: &HashMap<&str, GenericType<'a>>, types: &HashMap<&'a str, TypeMetaData<'a>>) -> Result<GenericType<'a>, TypeError> {
    match expression {
        Expression::Variable { name } => {
            // variable known in its environment?
            match environment.get(name) {
                Some(var_type) => Ok(var_type.clone()),
                None => Err(TypeError { message: report_unknown_variable(name) })
            }
        }
        Expression::MethodCall { .. } => {
            todo!()
        }
        Expression::StructLiteral { name, instantiation, field_expressions } => {
            match types.get(name) {
                Some(type_info) => {
                    match type_info {
                        TypeMetaData::Struct { bound, fields, .. } => {
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
                            check_type_bound(name, instantiation, &struct_literal_environment, types)?;

                            // correct amount of parameters supplied?
                            if field_expressions.len() != fields.len() {
                                return Err(TypeError { message: report_literal_wrong_argcount(name, fields.len(), field_expressions.len()) });
                            }

                            for (index, expression) in field_expressions.iter().enumerate() {
                                if let Some(field) = fields.get(index) {
                                    // evaluate type of supplied parameter
                                    let field_type = check_expression(expression, &struct_literal_environment, types)?;

                                    // field expression subtype of field type?
                                    is_subtype_of(&field_type, &field.type_, &struct_literal_environment, types)?;
                                }
                            }

                            Ok(GenericType::NamedType(name, instantiation.clone()))
                        }
                        TypeMetaData::Interface { .. } => Err(TypeError { message: report_struct_literal_interface(name) }),
                    }
                }
                None => Err(TypeError { message: report_unknown_struct_literal(name) }),
            }
        }
        Expression::Select { expression, field: field_var } => {
            // expression type is a struct type?
            let type_name = match check_expression(expression, environment, types)? {
                GenericType::NumberType => return Err(TypeError { message: report_select_number() }),
                GenericType::TypeParameter(_) => return Err(TypeError { message: report_select_type_parameter() }),
                GenericType::NamedType(name, _) => name,
            };

            match types.get(type_name).unwrap() {
                TypeMetaData::Struct { fields, .. } => {
                    // selected field part of the expression type?
                    match fields.iter().find(|field| &field.name == field_var) {
                        Some(field) => Ok(field.type_.clone()),
                        None => Err(TypeError { message: report_unknown_field(field_var, type_name) }),
                    }
                }
                TypeMetaData::Interface { .. } => Err(TypeError { message: report_select_interface() }),
            }
        }
        Expression::TypeAssertion { expression, assert } => {
            // asserted type well formed?
            check_type(assert, environment, types)?;

            // evaluate body expression
            let expression_type = check_expression(expression, environment, types)?;

            let body_type_metadata = match expression_type {
                GenericType::NumberType => {
                    return if assert == &GenericType::NumberType {
                        Ok(GenericType::NumberType)
                    } else {
                        Err(TypeError { message: report_assert_type_mismatch(type_name(&expression_type), type_name(assert)) })
                    };
                }
                _ => obtain_nested_typeinfo(&expression_type, environment, types)?,
            };

            let assert_type_metadata = match assert {
                GenericType::NumberType => {
                    return if expression_type == GenericType::NumberType {
                        Ok(GenericType::NumberType)
                    } else {
                        Err(TypeError { message: report_assert_type_mismatch(type_name(&expression_type), type_name(assert)) })
                    };
                }
                _ => obtain_nested_typeinfo(&expression_type, environment, types)?,
            };

            match (assert_type_metadata, body_type_metadata) {
                (TypeMetaData::Interface { .. }, TypeMetaData::Interface { .. }) => (),
                (TypeMetaData::Interface { .. }, TypeMetaData::Struct { .. }) => return Err(TypeError { message: report_assert_type_mismatch(type_name(&expression_type), type_name(assert)) }),
                (TypeMetaData::Struct { .. }, TypeMetaData::Interface { .. }) => {
                    is_subtype_of(assert, &expression_type, environment, types)?;
                }
                (TypeMetaData::Struct { .. }, TypeMetaData::Struct { .. }) => (),
            }

            Ok(assert.clone())
        }
        Expression::Number { .. } => {
            Ok(GenericType::NumberType)
        }
        Expression::BinOp { lhs, rhs, .. } => {
            // left side of operation number type?
            let lhs_type = check_expression(lhs, environment, types)?;

            // right side of operation number type?
            let rhs_type = check_expression(rhs, environment, types)?;

            match (lhs_type, rhs_type) {
                (GenericType::NumberType, GenericType::NumberType) => Ok(GenericType::NumberType),
                (_, _) => Err(TypeError { message: report_invalid_binop() }),
            }
        }
    }
}

pub(crate) fn is_subtype_of<'a>(child_type: &GenericType, parent_type: &GenericType, environment: &HashMap<&str, GenericType<'a>>, types: &HashMap<&'a str, TypeMetaData<'a>>) -> Result<(), TypeError> {
    // a type is a subtype of itself
    if parent_type == child_type {
        return Ok(());
    }

    if child_type == &GenericType::NumberType {
        return Err(TypeError { message: String::from("ERROR: An integer value cant be the child type of a struct value") });
    }

    let child_type_info = match types.get(type_name(child_type)) {
        Some(type_info) => type_info,
        None => {
            // if type is not found, check environment of receiver
            match environment.get(type_name(child_type)) {
                Some(type_) => {
                    types.get(type_name(type_)).expect("Type should be present in types")
                }
                None => {
                    return Err(TypeError { message: format!("ERROR: Use of undeclared type formal '{:?}'", child_type) });
                }
            }
        }
    };

    let parent_type_info = obtain_nested_typeinfo(parent_type, environment, types)?;

    let methods = match parent_type_info {
        TypeMetaData::Struct { .. } => {
            return Err(TypeError { message: String::from("ERROR: An struct value cant be the parent type") });
        }
        TypeMetaData::Interface { methods, .. } => methods,
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

fn obtain_nested_typeinfo<'a>(type_: &GenericType, environment: &HashMap<&str, GenericType>, types: &HashMap<&str, TypeMetaData<'a>>) -> Result<TypeMetaData<'a>, TypeError> {
    match types.get(type_name(type_)) {
        Some(type_info) => Ok(type_info.clone()),
        None => {
            match type_ {
                GenericType::NumberType => Err(TypeError { message: report_wrong_type_bound() }),
                _ => {
                    match environment.get(type_name(type_)) {
                        Some(nested_type) => Ok(obtain_nested_typeinfo(nested_type, environment, types)?),
                        None => Err(TypeError { message: report_unknown_type_formal(type_name(type_)) }),
                    }
                }
            }
        }
    }
}

/*
    Check if type bound is satisfied

        - correct amount of parameters supplied
        - instantiated types well formed in its environment
        - instantiated type subtype of type bound
 */
fn check_type_bound<'a>(type_: &'a str, instantiation: &Vec<GenericType<'a>>, environment: &HashMap<&'a str, GenericType<'a>>, types: &HashMap<&'a str, TypeMetaData<'a>>) -> Result<(), TypeError> {
    let type_info = types.get(type_).expect("Occurrence of type was checked beforehand");

    match type_info {
        TypeMetaData::Struct { bound, .. } => {
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
        TypeMetaData::Interface { .. } => return Err(TypeError { message: report_interfaces_forbidden() }),
    }

    Ok(())
}

fn type_name<'a>(type_: &'a GenericType) -> &'a str {
    match type_ {
        GenericType::TypeParameter(type_parameter) => type_parameter,
        GenericType::NamedType(name, _) => name,
        GenericType::NumberType => "int",
    }
}

