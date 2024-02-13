use std::collections::{HashMap, HashSet};
use std::hash::{Hash};
use parser::{Expression, GenericType};
use type_checker::{check_expression, TypeEnvironment, TypeError, TypeInfos, VariableEnvironment};

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
    let instance_set = instance_set_of(expression, &mut HashMap::new(), &mut HashMap::new(), type_infos)?;

    apply_g_function(&instance_set)?;

    Ok(())
}

fn instance_set_of<'a>(
    expression: &'a Expression<'a>,
    variable_environment: &mut VariableEnvironment<'a>,
    type_environment: &mut TypeEnvironment<'a>,
    type_infos: &TypeInfos<'a>,
) -> Result<HashSet<InstanceType<'a>>, TypeError> {
    match expression {
        Expression::Variable { .. } => Ok(HashSet::new()),
        Expression::MethodCall { expression, method, instantiation, parameter_expressions } => {
            let mut method_instance_set = HashSet::new();

            // add instance type of method to set
            let expression_type = check_expression(expression, variable_environment, type_environment, type_infos, method)?;
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

fn apply_g_function(instance_set: &HashSet<InstanceType>) -> Result<(), TypeError> {
    println!("{:#?}", instance_set);
    Ok(())
}