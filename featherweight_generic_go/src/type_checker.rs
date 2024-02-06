use std::collections::{HashMap};
use diagnostics::{report_assert_type_mismatch, report_duplicate_field_name, report_duplicate_method_implementation, report_duplicate_method_name, report_duplicate_method_parameter, report_duplicate_type, report_interface_implementation, report_invalid_binop, report_literal_wrong_argcount, report_select_interface, report_struct_literal_interface, report_subtype_type_mismatch, report_unknown_field, report_unknown_receiver_type, report_unknown_struct_literal, report_unknown_type, report_unknown_type_parameter, report_unknown_variable, report_wrong_type_bound};
use parser::{Declaration, Expression, GenericBinding, GenericReceiver, GenericType, MethodDeclaration, MethodSpecification, Program, TypeLiteral};

// TODO Self recursion in struct
// TODO Präsi (Wann, Inhalt, Ablauf, Gespräch danach?)
// TODO Distinct Check Parameter name und Type parameter?

// Type name -> Type Info
type TypeInfos<'a> = HashMap<&'a str, TypeInfo<'a>>;

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
                return Err(TypeError { message: report_duplicate_type(type_name) });
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
    check_expression(&program.expression, &HashMap::new(), &mut HashMap::new(), type_infos)
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
fn check_type_literal(name: &str, bound: &[GenericBinding], literal: &TypeLiteral, type_infos: &TypeInfos) -> Result<(), TypeError> {
    // create environment for the type formals
    let mut type_environment = HashMap::new();

    // type formals well-formed in the empty type environment?
    for binding in bound {
        match type_infos.get(binding.type_.name()) {
            Some(type_info) => {
                match type_info {
                    TypeInfo::Struct { .. } => return Err(TypeError { message: report_wrong_type_bound() }),
                    TypeInfo::Interface { .. } => _ = type_environment.insert(binding.name, binding.type_.clone()),
                }
            }
            None => return Err(TypeError { message: format!("Encountered unknown type '{}' for type parameter '{}' in literal '{name}'", binding.type_.name(), binding.name) })
        }
    }

    match literal {
        TypeLiteral::Struct { fields } => {
            for (index, field) in fields.iter().enumerate() {
                // field names distinct?
                if fields.iter().skip(index + 1).any(|binding| binding.name == field.name) {
                    return Err(TypeError { message: report_duplicate_field_name(field.name, name) });
                }

                // field type well-formed in the literal environment?
                check_type(&field.type_, &type_environment, type_infos)?;
            }
        }
        TypeLiteral::Interface { methods } => {
            for (index, method_specification) in methods.iter().enumerate() {
                // method names unique?
                if methods.iter().skip(index + 1).any(|method_spec| method_spec.name == method_specification.name) {
                    return Err(TypeError { message: report_duplicate_method_name(method_specification.name, name) });
                }

                // method specification well-formed in the literal environment?
                check_method_specification(method_specification, &type_environment, type_infos)?;
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
fn check_method_specification(specification: &MethodSpecification, literal_environment: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), TypeError> {
    // create environment for method type formals
    let mut method_environment = HashMap::new();

    for binding in &specification.bound {
        // type formals of the method well-formed in the type literal environment?
        check_type(&binding.type_, literal_environment, type_infos)?;

        // only interface types can be a type bound
        match type_infos.get(binding.type_.name()) {
            Some(type_info) => {
                match type_info {
                    TypeInfo::Struct { .. } => return Err(TypeError { message: report_wrong_type_bound() }),
                    TypeInfo::Interface { .. } => _ = method_environment.insert(binding.name, binding.type_.clone()),
                }
            }
            None => return Err(TypeError { message: format!("Encountered unknown type '{}' for type formal '{}'", binding.type_.name(), binding.name) })
        }
    }

    let type_environment = concat_type_environments(literal_environment, &method_environment, type_infos)?;

    for (index, parameter) in specification.parameters.iter().enumerate() {
        // parameter names distinct?
        if specification.parameters.iter().skip(index + 1).any(|element| element.name == parameter.name) {
            return Err(TypeError { message: report_duplicate_method_parameter(parameter.name, specification.name) });
        }

        // parameter type well-formed in the concatenated environment?
        check_type(&parameter.type_, &type_environment, type_infos)?;
    }

    // return type well-formed in the concatenated environment?
    check_type(&specification.return_type, &type_environment, type_infos)?;

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
fn check_method(receiver: &GenericReceiver, specification: &MethodSpecification, body: &Expression, type_infos: &TypeInfos) -> Result<(), TypeError> {
    for (index, parameter) in specification.parameters.iter().enumerate() {
        // receiver name and parameter names distinct?
        if receiver.name == parameter.name || specification.parameters.iter().skip(index + 1).any(|binding| binding.name == parameter.name) {
            return Err(TypeError { message: report_duplicate_method_parameter(parameter.name, specification.name) });
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
                            TypeInfo::Struct { .. } => return Err(TypeError { message: report_wrong_type_bound() }),
                            TypeInfo::Interface { .. } => _ = receiver_environment.insert(binding.name, binding.type_.clone()),
                        }
                    }
                    None => return Err(TypeError { message: format!("Encountered unknown type '{}' for type formal '{}'", binding.type_.name(), binding.name) })
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
                            TypeInfo::Struct { .. } => return Err(TypeError { message: report_wrong_type_bound() }),
                            TypeInfo::Interface { .. } => _ = method_environment.insert(binding.name, binding.type_.clone()),
                        }
                    }
                    None => return Err(TypeError { message: format!("Encountered unknown type '{}' for type formal '{}'", binding.type_.name(), binding.name) })
                }
            }

            let mut type_environment = concat_type_environments(&receiver_environment, &method_environment, type_infos)?;

            // parameter types well-formed in the concatenated environment?
            for parameter in &specification.parameters {
                check_type(&parameter.type_, &type_environment, type_infos)?;
            }

            // return-type well-formed in the concatenated environment??
            check_type(&specification.return_type, &type_environment, type_infos)?;

            // create an environment for the variables
            let mut variable_environment = HashMap::new();

            variable_environment.insert(receiver.name, receiver_type);

            for parameter in &specification.parameters {
                variable_environment.insert(parameter.name, parameter.type_.clone());
            }

            // body expression well-formed in the concatenated environment?
            let expression_type = check_expression(body, &variable_environment, &mut type_environment, type_infos)?;

            // type of body expression subtype of return type in the concatenated environment?
            is_subtype_of(&expression_type, &specification.return_type, &type_environment, type_infos)?;

            Ok(())
        }
        None => Err(TypeError { message: format!("Tried to implement method '{}' for undeclared receiver type '{}'", specification.name, receiver.type_) })
    }
}

/*
    Checks if type t is well-formed

        - all type parameters must be declared in its environment
        - all named types must be instantiated with type arguments that satisfy its bounds
*/
fn check_type(type_: &GenericType, type_environment: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), TypeError> {
    match type_ {
        GenericType::TypeParameter(type_parameter) => {
            // type parameter declared?
            if !type_environment.contains_key(type_parameter) {
                return Err(TypeError { message: report_unknown_type_parameter(type_parameter) });
            }
        }
        GenericType::NamedType(type_name, instantiation) => {
            if !type_infos.contains_key(type_name) {
                return Err(TypeError { message: report_unknown_type(type_name) });
            }

            for parameter in instantiation {
                // parameter well-formed in its environment?
                check_type(parameter, type_environment, type_infos)?;
            }

            // type bound satisfied?
            check_type_bound(type_name, instantiation, type_environment, type_infos)?;
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
fn check_expression<'a>(
    expression: &Expression<'a>,
    variable_environment: &VariableEnvironment<'a>,
    type_environment: &mut TypeEnvironment<'a>,
    type_infos: &TypeInfos<'a>,
) -> Result<GenericType<'a>, TypeError> {
    match expression {
        Expression::Variable { name } => {
            // variable known?
            match variable_environment.get(name) {
                Some(var_type) => Ok(var_type.clone()),
                None => Err(TypeError { message: report_unknown_variable(name) })
            }
        }
        Expression::MethodCall { .. } => {
            todo!()
        }
        Expression::StructLiteral { name, instantiation, field_expressions } => {
            let struct_type = GenericType::NamedType(name, instantiation.clone());

            // struct type well-formed?
            check_type(&struct_type, type_environment, type_infos)?;

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
                            if field_expressions.len() != fields.len() {
                                return Err(TypeError { message: report_literal_wrong_argcount(name, fields.len(), field_expressions.len()) });
                            }

                            for (index, expression) in field_expressions.iter().enumerate() {
                                if let Some(field) = fields.get(index) {
                                    // evaluate type of supplied parameter
                                    let field_type = check_expression(expression, variable_environment, type_environment, type_infos)?;

                                    // field expression subtype of field type?
                                    is_subtype_of(&field_type, &field.type_, type_environment, type_infos)?;
                                }
                            }

                            Ok(struct_type)
                        }
                        TypeInfo::Interface { .. } => Err(TypeError { message: report_struct_literal_interface(name) }),
                    }
                }
                None => Err(TypeError { message: report_unknown_struct_literal(name) }),
            }
        }
        Expression::Select { expression, field: field_var } => {
            let expression_type = check_expression(expression, variable_environment, type_environment, type_infos)?;

            match type_infos.get(expression_type.name()).unwrap() {
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
            // asserted type well-formed?
            check_type(assert, type_environment, type_infos)?;

            // evaluate body expression
            let expression_type = check_expression(expression, variable_environment, type_environment, type_infos)?;

            match (&expression_type, assert) {
                (GenericType::NumberType, GenericType::NumberType) => return Ok(GenericType::NumberType),
                (GenericType::NumberType, _) => return Err(TypeError { message: report_assert_type_mismatch(expression_type.name(), assert.name()) }),
                (_, GenericType::NumberType) => return Err(TypeError { message: report_assert_type_mismatch(expression_type.name(), assert.name()) }),
                (GenericType::NamedType(expression_type_name, _), GenericType::NamedType(assert_type_name, _)) => {
                    let expression_type_info = type_infos.get(expression_type_name).unwrap();
                    let assert_type_info = type_infos.get(assert_type_name).unwrap();

                    match (expression_type_info, assert_type_info) {
                        (TypeInfo::Interface { .. }, TypeInfo::Interface { .. }) => (),
                        (TypeInfo::Struct { .. }, TypeInfo::Struct { .. }) => (),
                        (TypeInfo::Interface { .. }, TypeInfo::Struct { .. }) => return Err(TypeError { message: report_assert_type_mismatch(expression_type.name(), assert.name()) }),
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
                        (TypeInfo::Interface { .. }, TypeInfo::Struct { .. }) => return Err(TypeError { message: report_assert_type_mismatch(expression_type.name(), assert.name()) }),
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
            let lhs_type = check_expression(lhs, variable_environment, type_environment, type_infos)?;

            // right side of operation number type?
            let rhs_type = check_expression(rhs, variable_environment, type_environment, type_infos)?;

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
pub(crate) fn is_subtype_of(child_type: &GenericType, parent_type: &GenericType, type_environment: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), TypeError> {
    // a type is a subtype of itself
    if child_type.name() == parent_type.name() {
        return Ok(());
    }

    match (child_type, parent_type) {
        (GenericType::NumberType, GenericType::NumberType) => (),
        (GenericType::NumberType, _) => return Err(TypeError { message: report_subtype_type_mismatch(child_type.name(), parent_type.name()) }),
        (_, GenericType::NumberType) => return Err(TypeError { message: report_subtype_type_mismatch(child_type.name(), parent_type.name()) }),
        (GenericType::NamedType(child_name, _), GenericType::NamedType(parent_name, _)) => {
            let child_type_info = type_infos.get(child_name).unwrap();
            let parent_type_info = type_infos.get(parent_name).unwrap();

            match (child_type_info, parent_type_info) {
                (TypeInfo::Struct { .. }, TypeInfo::Struct { .. }) => {
                    if child_name != parent_name {
                        return Err(TypeError { message: report_subtype_type_mismatch(child_type.name(), parent_type.name()) });
                    }
                }
                (TypeInfo::Interface { .. }, TypeInfo::Struct { .. }) => {
                    return Err(TypeError { message: report_subtype_type_mismatch(child_type.name(), parent_type.name()) });
                }
                _ => implements_type(child_type, child_type_info, parent_type, parent_type_info)?,
            }
        }
        (_, GenericType::TypeParameter(parent_name)) => {
            let parent_type = type_environment.get(parent_name).unwrap();
            return is_subtype_of(child_type, parent_type, type_environment, type_infos);
        }
        (GenericType::TypeParameter(child_name), _) => {
            let child_type = type_environment.get(child_name).unwrap();
            return is_subtype_of(child_type, parent_type, type_environment, type_infos);
        }
    }

    Ok(())
}

fn implements_type(child_type: &GenericType, child_type_info: &TypeInfo, parent_type: &GenericType, parent_type_info: &TypeInfo) -> Result<(), TypeError> {
    let methods = match parent_type_info {
        TypeInfo::Interface { methods, .. } => methods,
        _ => unreachable!(),
    };

    for method in methods.iter() {
        match child_type_info.method_specification(method.name) {
            None => {
                return Err(TypeError { message: format!("ERROR: Method '{}' of parent type '{}' is not implemented for child type '{}'", method.name, parent_type.name(), child_type.name()) });
            }
            Some(method_spec) => {
                if method.return_type != method_spec.return_type {
                    return Err(TypeError { message: format!("ERROR: Method '{}' of parent type '{}' has return type '{}' but return type of child implementation is '{}'", method.name, parent_type.name(), method.return_type.name(), method_spec.return_type.name()) });
                }

                if method.parameters.len() != method_spec.parameters.len() {
                    return Err(TypeError { message: format!("ERROR: Method {} of parent type {:?} has {:?} parameters but child implementation has {:?} parameters", method.name, parent_type, method.parameters.len(), method_spec.parameters.len()) });
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

fn concat_type_environments<'a>(outer: &TypeEnvironment<'a>, inner: &TypeEnvironment<'a>, type_infos: &TypeInfos) -> Result<VariableEnvironment<'a>, TypeError> {
    let mut concat_environment = HashMap::new();

    // types in the outer environment well-formed in the empty environment?
    for (name, type_) in outer {
        check_type(type_, &HashMap::new(), type_infos)?;

        concat_environment.insert(*name, type_.clone());
    }

    // types in the inner environment well formed in the outer environment?
    for (name, type_) in inner {
        check_type(type_, outer, type_infos)?;

        match concat_environment.insert(*name, type_.clone()) {
            Some(_) => return Err(TypeError { message: format!("Duplicate type formal '{name}'") }),
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
fn check_type_bound(type_: &str, instantiation: &Vec<GenericType>, type_environment: &TypeEnvironment, types: &TypeInfos) -> Result<(), TypeError> {
    match types.get(type_).unwrap() {
        TypeInfo::Struct { bound, .. } => {
            // correct amount of parameters supplied?
            if bound.len() != instantiation.len() {
                return Err(TypeError { message: format!("ERROR: Type '{type_}' has {} generic parameters but {} parameters were supplied", bound.len(), instantiation.len()) });
            }

            // bound = formal type
            // instantiation = actual types
            for (index, parameter) in bound.iter().enumerate() {
                let actual_type = instantiation.get(index).unwrap();

                // actual type well-formed in its environment?
                check_type(actual_type, type_environment, types)?;

                // instantiated type subtype of type bound?
                is_subtype_of(actual_type, &parameter.type_, type_environment, types)?;
            }
        }
        TypeInfo::Interface { bound, .. } => {
            // correct amount of parameters supplied?
            if bound.len() != instantiation.len() {
                return Err(TypeError { message: format!("ERROR: Type '{type_}' has '{}' generic parameters but '{}' parameters were supplied", bound.len(), instantiation.len()) });
            }

            for (index, parameter) in bound.iter().enumerate() {
                let instantiated_type = instantiation.get(index).unwrap();

                // instantiated type well-formed in its environment?
                check_type(instantiated_type, type_environment, types)?;

                // instantiated type subtype of type bound?
                is_subtype_of(instantiated_type, &parameter.type_, type_environment, types)?;
            }
        }
    }

    Ok(())
}