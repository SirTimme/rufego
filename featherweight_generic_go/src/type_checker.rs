use std::collections::{HashMap};
use common::parser::{Declaration, Expression, GenericBinding, GenericReceiver, GenericType, MethodDeclaration, MethodSpecification, Program, RufegoError, TypeLiteral};
use common::{expression_well_formed, is_subtype_of, type_well_formed, TypeEnvironment, TypeInfo, TypeInfos};


pub(crate) fn program_well_formed<'a, 'b>(program: &'a Program<'b>, type_infos: &'a TypeInfos<'b>) -> Result<GenericType<'b>, RufegoError> {
    // declarations well-formed?
    for declaration in &program.declarations {
        declaration_well_formed(declaration, type_infos)?;
    }

    // body expression well-formed in the empty type environment and empty variable environment?
    match expression_well_formed(&program.expression, &HashMap::new(), &HashMap::new(), type_infos) {
        Ok(type_) => Ok(type_),
        Err(rufego_error) => {
            Err(RufegoError { message: format!("Body expression of 'main' is not well-formed:\n{}", rufego_error.message) })
        }
    }
}

fn declaration_well_formed(declaration: &Declaration, type_infos: &TypeInfos) -> Result<(), RufegoError> {
    match declaration {
        // method declaration well-formed?
        Declaration::Method(MethodDeclaration { receiver, specification, body }) => {
            match method_well_formed(receiver, specification, body, type_infos) {
                Ok(_) => {}
                Err(error) => {
                    let error_message = format!(
                        "Method '{}' for receiver type '{}' is not well-formed:\n{}",
                        specification.name,
                        receiver.type_,
                        error.message
                    );
                    return Err(RufegoError { message: error_message });
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
                    return Err(RufegoError { message: format!("Type formals of literal '{name}' is not well-formed:\n{}", error.message) });
                }
            }

            // type literal well-formed?
            match type_literal_well_formed(literal, &psi, type_infos) {
                Ok(_) => {}
                Err(error) => {
                    return Err(RufegoError { message: format!("Literal '{name}' is not well-formed:\n{}", error.message) });
                }
            }
        }
    }

    Ok(())
}

fn type_literal_well_formed(literal: &TypeLiteral, literal_environment: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), RufegoError> {
    match literal {
        TypeLiteral::Struct { fields } => struct_well_formed(fields, literal_environment, type_infos)?,
        TypeLiteral::Interface { methods } => interface_well_formed(methods, literal_environment, type_infos)?,
    }

    Ok(())
}

fn struct_well_formed(fields: &[GenericBinding], type_environment: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), RufegoError> {
    for (index, field) in fields.iter().enumerate() {
        // field names distinct?
        if fields.iter().skip(index + 1).any(|binding| binding.name == field.name) {
            return Err(RufegoError { message: format!("Duplicate field with name '{}'", field.name) });
        }

        // field type well-formed in the literal environment?
        type_well_formed(&field.type_, type_environment, type_infos)?;
    }

    Ok(())
}

fn interface_well_formed(methods: &[MethodSpecification], type_environment: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), RufegoError> {
    for (index, method_specification) in methods.iter().enumerate() {
        // name of method specification unique?
        if methods.iter().skip(index + 1).any(|method_spec| method_spec.name == method_specification.name) {
            return Err(RufegoError { message: format!("Duplicate method specification with name '{}'", method_specification.name) });
        }

        // method specification well-formed in the literal environment?
        match method_specification_well_formed(method_specification, type_environment, type_infos) {
            Ok(_) => {}
            Err(rufego_error) => {
                return Err(RufegoError { message: format!("Method specification '{}' is not well-formed: {}", method_specification.name, rufego_error.message) });
            }
        }
    }

    Ok(())
}

fn method_specification_well_formed(specification: &MethodSpecification, literal_environment: &TypeEnvironment, type_infos: &TypeInfos) -> Result<(), RufegoError> {
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
            return Err(RufegoError { message: format!("Duplicate method parameter with name '{}'", parameter.name) });
        }

        // parameter type well-formed in the concatenated environment?
        match type_well_formed(&parameter.type_, &delta, type_infos) {
            Ok(_) => {}
            Err(error) => {
                let error_message = format!(
                    "Method parameter '{}' with type '{}' is not well-formed:\n{}",
                    parameter.name,
                    parameter.type_.name(),
                    error.message
                );

                return Err(RufegoError { message: error_message });
            }
        }
    }

    // return type well-formed in the concatenated environment?
    match type_well_formed(&specification.return_type, &delta, type_infos) {
        Ok(_) => {}
        Err(error) => {
            return Err(RufegoError { message: format!("Return type '{}' is not well-formed:\n{}", specification.return_type.name(), error.message) });
        }
    }

    Ok(())
}

fn method_well_formed(receiver: &GenericReceiver, specification: &MethodSpecification, body: &Expression, type_infos: &TypeInfos) -> Result<(), RufegoError> {
    for (index, parameter) in specification.parameters.iter().enumerate() {
        // name of receiver type and parameter distinct?
        if receiver.name == parameter.name || specification.parameters.iter().skip(index + 1).any(|binding| binding.name == parameter.name) {
            return Err(RufegoError { message: format!("Duplicate method parameter with name '{}'", parameter.name) });
        }
    }

    // receiver type declared?
    let receiver_type_info = match type_infos.get(receiver.type_) {
        None => {
            return Err(RufegoError { message: format!("Receiver type '{}' is not declared", receiver.type_) });
        }
        Some(type_info) => {
            type_info
        }
    };

    match receiver_type_info {
        TypeInfo::Struct { bound, .. } => {
            if receiver.instantiation.len() != bound.len() {
                let error_message = format!(
                    "Receiver type '{}' expects '{}' type parameters, but '{}' type parameters were provided",
                    receiver.type_,
                    bound.len(),
                    receiver.instantiation.len()
                );
                return Err(RufegoError { message: error_message });
            }

            match type_formals_subtype_of(&receiver.instantiation, bound, type_infos) {
                Ok(_) => {}
                Err(error) => {
                    let error_message = format!(
                        "Receiver bound of type '{}' for method '{}' is not a subtype of declared bound: {}",
                        receiver.type_,
                        specification.name,
                        error.message
                    );

                    return Err(RufegoError { message: error_message });
                }
            }
        }
        TypeInfo::Interface { .. } => {
            return Err(RufegoError { message: format!("Receiver type '{}' is an interface type", receiver.type_) });
        }
    }

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
    let expression_type = match expression_well_formed(body, &variable_environment, &delta, type_infos) {
        Ok(expression_type) => {
            expression_type
        }
        Err(error) => {
            return Err(RufegoError { message: format!("Body expression of method is not well-formed:\n{}", error.message) });
        }
    };

    // body expression subtype of return type in the concatenated environment?
    match is_subtype_of(&expression_type, &specification.return_type, &delta, type_infos) {
        Ok(_) => {}
        Err(error) => {
            let error_message = format!(
                "Body expression type '{}' is not a subtype of declared return type '{}':\n{}",
                expression_type.name(),
                specification.return_type.name(),
                error.message
            );
            return Err(RufegoError { message: error_message });
        }
    }

    Ok(())
}

fn type_formals_subtype_of(instantiation: &[GenericBinding], bound: &[GenericBinding], type_infos: &TypeInfos) -> Result<(), RufegoError> {
    for (index, bound_binding) in bound.iter().enumerate() {
        let instantiation_binding = instantiation.get(index).unwrap();

        is_subtype_of(&instantiation_binding.type_, &bound_binding.type_, &TypeEnvironment::new(), type_infos)?;
    }

    Ok(())
}

fn formal_type_well_formed<'a>(outer: &TypeEnvironment<'a>, inner: &TypeEnvironment<'a>, type_infos: &TypeInfos) -> Result<TypeEnvironment<'a>, RufegoError> {
    let mut concat_environment = HashMap::new();

    // insert types of outer environment to concatenated type environment
    for (name, type_) in outer.iter() {
        concat_environment.insert(*name, type_.clone());
    }

    for (name, type_) in inner.iter() {
        // duplicate type parameter names?
        match concat_environment.insert(*name, type_.clone()) {
            Some(_) => return Err(RufegoError { message: format!("Duplicate type parameter with name '{name}'") }),
            None => continue,
        }
    }

    // types of the inner environment well-formed in the concatenated environment?
    for type_ in inner.values() {
        type_well_formed(type_, &concat_environment, type_infos)?;
    }

    Ok(concat_environment)
}

fn nested_type_formals_well_formed<'a>(outer: &TypeEnvironment<'a>, inner: &TypeEnvironment<'a>, type_infos: &TypeInfos) -> Result<TypeEnvironment<'a>, RufegoError> {
    let _ = formal_type_well_formed(&HashMap::new(), outer, type_infos)?;
    let delta = formal_type_well_formed(outer, inner, type_infos)?;

    Ok(delta)
}
