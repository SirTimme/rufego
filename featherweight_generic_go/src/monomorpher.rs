use std::collections::{HashMap, HashSet};
use std::hash::{Hash};
use parser::{Expression, GenericType};
use type_checker::{expression_well_formed, generate_substitution, is_subtype_of, methods_of_type, substitute_struct_fields, substitute_type_parameter, TypeEnvironment, TypeError, TypeInfo, TypeInfos, VariableEnvironment};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum InstanceType<'a> {
    Type {
        type_: GenericType<'a>,
    },
    Method {
        type_: GenericType<'a>,
        method_name: &'a str,
        instantiation: Vec<GenericType<'a>>,
    },
}

pub(crate) fn monomorph<'a, 'b>(expression: &'a Expression<'b>, type_infos: &'a TypeInfos<'b>) -> Result<(), TypeError> {
    let mut instance_set = instance_set_of(expression, &HashMap::new(), &HashMap::new(), type_infos)?;
    let mut instance_set_size = instance_set.len();
    let delta = TypeEnvironment::new();
    
    loop {
        let iteration_result = g_function(&instance_set, &delta, type_infos)?;

        for value in iteration_result {
            if !instance_set.contains(&value) {
                instance_set.insert(value);
            }
        }

        if instance_set.len() == instance_set_size {
            break;
        }

        instance_set_size = instance_set.len();
    }

    println!("{:#?}", instance_set);

    Ok(())
}

fn instance_set_of<'a, 'b>(
    expression: &'a Expression<'b>,
    variable_environment: &'a VariableEnvironment<'b>,
    type_environment: &'a TypeEnvironment<'b>,
    type_infos: &'a TypeInfos<'b>,
) -> Result<HashSet<InstanceType<'b>>, TypeError> {
    match expression {
        Expression::Variable { .. } => Ok(HashSet::new()),
        Expression::MethodCall { expression, method, instantiation, parameter_expressions } => {
            let mut method_instance_set = HashSet::new();

            // add instance type of method to set
            let expression_type = expression_well_formed(expression, variable_environment, type_environment, type_infos)?;

            let method_expression_type = InstanceType::Type { type_: expression_type.clone() };

            method_instance_set.insert(method_expression_type);

            // add method call to instance set
            let method_instance_type = InstanceType::Method {
                type_: expression_type.clone(),
                method_name: method,
                instantiation: instantiation.clone(),
            };

            method_instance_set.insert(method_instance_type);

            // add instance set of expression to set
            let expression_instance_set = instance_set_of(expression, variable_environment, type_environment, type_infos)?;
            method_instance_set.extend(expression_instance_set);

            // add instance set of each method parameter to set
            for parameter in parameter_expressions {
                let parameter_instance_set = instance_set_of(parameter, variable_environment, type_environment, type_infos)?;
                method_instance_set.extend(parameter_instance_set);
            }

            Ok(method_instance_set)
        }
        Expression::StructLiteral { name, instantiation, field_expressions } => {
            let mut struct_instance_set = HashSet::new();

            let struct_type = GenericType::NamedType(name, instantiation.clone());
            let struct_instance_type = InstanceType::Type { type_: struct_type };

            struct_instance_set.insert(struct_instance_type);

            for field_expression in field_expressions {
                let field_instance_set = instance_set_of(field_expression, variable_environment, type_environment, type_infos)?;
                struct_instance_set.extend(field_instance_set);
            }

            Ok(struct_instance_set)
        }
        Expression::Select { expression, .. } => {
            Ok(instance_set_of(expression, variable_environment, type_environment, type_infos)?)
        }
        Expression::TypeAssertion { expression, assert } => {
            let expression_instance_set = instance_set_of(expression, variable_environment, type_environment, type_infos)?;

            let mut assert_instance_set = HashSet::new();

            assert_instance_set.insert(InstanceType::Type { type_: assert.clone() });
            assert_instance_set.extend(expression_instance_set);

            Ok(assert_instance_set)
        }
        _ => {
            let mut number_instance_set = HashSet::new();
            number_instance_set.insert(InstanceType::Type { type_: GenericType::NumberType });

            Ok(number_instance_set)
        }
    }
}

fn g_function<'a, 'b>(
    instance_set: &'a HashSet<InstanceType<'b>>,
    delta: &'a TypeEnvironment<'b>,
    type_infos: &'a TypeInfos<'b>,
) -> Result<HashSet<InstanceType<'b>>, TypeError> where 'a: 'b {
    let mut omega = HashSet::new();

    for element in instance_set {
        match element {
            InstanceType::Type { type_ } => {
                let f_result = f_closure(type_, type_infos)?;
                omega.extend(f_result);
            }
            InstanceType::Method { type_, method_name, instantiation } => {
                let m_result = m_closure(type_, method_name, instantiation, delta, type_infos)?;
                omega.extend(m_result);

                let i_result = i_closure(type_, method_name, instantiation, delta, type_infos, instance_set)?;
                omega.extend(i_result);
            }
        }
    }

    Ok(omega)
}

fn f_closure<'a, 'b>(type_: &'a GenericType<'b>, type_infos: &'a TypeInfos<'b>) -> Result<HashSet<InstanceType<'b>>, TypeError> {
    let mut result_set = HashSet::new();

    if let GenericType::NamedType(name, instantiation) = type_ {
        let type_info = type_infos.get(name).unwrap();

        if let TypeInfo::Struct { bound, fields, .. } = type_info {
            let substitution = generate_substitution(bound, instantiation)?;

            let substituted_fields = substitute_struct_fields(&substitution, fields)?;

            for field_binding in substituted_fields {
                result_set.insert(InstanceType::Type { type_: field_binding.type_.clone() });
            }
        }
    }

    Ok(result_set)
}

fn m_closure<'a, 'b>(
    type_: &'a GenericType<'b>,
    method_name: &'a str,
    instantiation: &'a Vec<GenericType<'b>>,
    delta: &'a TypeEnvironment<'b>,
    type_infos: &'a TypeInfos<'b>,
) -> Result<HashSet<InstanceType<'b>>, TypeError> {
    let mut result_set = HashSet::new();
    let methods = methods_of_type(type_, delta, type_infos)?;

    if let Some(method_specification) = methods.iter().find(|method_specification| method_specification.name == method_name) {
        let substitution = generate_substitution(&method_specification.bound, instantiation)?;

        for parameter in &method_specification.parameters {
            let substituted_parameter = substitute_type_parameter(&parameter.type_, &substitution);
            result_set.insert(InstanceType::Type { type_: substituted_parameter });
        }

        let substituted_return_type = substitute_type_parameter(&method_specification.return_type, &substitution);
        result_set.insert(InstanceType::Type { type_: substituted_return_type });
    }

    Ok(result_set)
}

fn i_closure<'a, 'b>(
    type_: &'a GenericType<'b>,
    method_name: &'a str,
    instantiation: &'a [GenericType<'b>],
    delta: &'a TypeEnvironment<'b>,
    type_infos: &'a TypeInfos<'b>,
    omega: &'a HashSet<InstanceType<'b>>,
) -> Result<HashSet<InstanceType<'b>>, TypeError> where 'a: 'b {
    let mut result_set = HashSet::new();

    for value in omega {
        if let InstanceType::Type { type_: instance_type } = value {
            match is_subtype_of(instance_type, type_, delta, type_infos) {
                Ok(_) => {
                    result_set.insert(InstanceType::Method {
                        type_: instance_type.clone(),
                        method_name,
                        instantiation: Vec::from(instantiation),
                    });
                }
                Err(_) => {
                    continue;
                }
            }
        }
    }

    Ok(result_set)
}

