use std::collections::{HashMap, HashSet};
use std::hash::{Hash};
use parser::{Expression, GenericType};
use type_checker::{expression_well_formed, TypeEnvironment, TypeError, TypeInfo, TypeInfos, VariableEnvironment};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum InstanceType<'a> {
    Type {
        type_: GenericType<'a>,
    },
    Method {
        type_: GenericType<'a>,
        method_name: &'a str,
        type_arguments: Vec<GenericType<'a>>,
    },
}

pub(crate) fn monomorph(expression: &Expression, type_infos: &TypeInfos) -> Result<(), TypeError> {
    let inital_variable_set = HashMap::new();
    let initial_type_set = HashMap::new();
    
    let mut instance_set = instance_set_of(expression, &inital_variable_set, &initial_type_set, type_infos)?;

    loop {
        let result = apply_g_function(&instance_set, type_infos)?;

        if HashSet::is_empty(&result) {
            break;
        }

        instance_set.extend(result);
    }
    
    println!("{:#?}", instance_set);

    Ok(())
}

fn instance_set_of<'a>(
    expression: &'a Expression<'a>,
    variable_environment: &'a VariableEnvironment,
    type_environment: &'a TypeEnvironment,
    type_infos: &'a TypeInfos<'a>,
) -> Result<HashSet<InstanceType<'a>>, TypeError> {
    match expression {
        Expression::Variable { .. } => Ok(HashSet::new()),
        Expression::MethodCall { expression, method, instantiation, parameter_expressions } => {
            let mut method_instance_set = HashSet::new();

            // add instance type of method to set
            let expression_type = expression_well_formed(expression, variable_environment, type_environment, type_infos, method)?;
            let method_expression_type = InstanceType::Type { type_: expression_type.clone() };
            method_instance_set.insert(method_expression_type);

            // add method call to instance set
            let method_instance_type = InstanceType::Method {
                type_: expression_type.clone(),
                method_name: method,
                type_arguments: instantiation.clone(),
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

fn apply_g_function<'a>(instance_set: &HashSet<InstanceType>, type_infos: &'a TypeInfos) -> Result<HashSet<InstanceType<'a>>, TypeError> {
    let mut result_set = HashSet::new();

    for element in instance_set {
        // apply f
        // apply m 
        // apply i 
        // apply s
        
        let element_set = match element {
            InstanceType::Type { type_ } => f_closure(type_, type_infos)?,
            InstanceType::Method { .. } => HashSet::new(),
        };

        for result_element in element_set {
            if !instance_set.contains(&result_element) {
                result_set.insert(result_element);
            }
        }
    }

    Ok(result_set)
}

fn f_closure<'a>(type_: &GenericType, type_infos: &'a TypeInfos) -> Result<HashSet<InstanceType<'a>>, TypeError> {
    let mut result_set = HashSet::new();

    match type_ {
        GenericType::NamedType(name, ..) => {
            let type_info = type_infos.get(name).unwrap();

            match type_info {
                TypeInfo::Struct { fields, .. } => {
                    for field in fields.iter() {
                        result_set.insert(InstanceType::Type { type_: field.type_.clone() });
                    }
                }
                TypeInfo::Interface { .. } => (),
            }
        }
        _ => _ = result_set.insert(InstanceType::Type { type_: GenericType::NumberType }),
    }

    Ok(result_set)
}