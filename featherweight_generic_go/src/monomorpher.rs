use std::collections::{HashMap, HashSet};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use common::{RufegoError};
use interpreter::{body_of, concat_substitutions};
use parser::{Expression, GenericBinding, GenericType, MethodDeclaration, MethodSpecification, Program};
use type_checker::{expression_well_formed, generate_substitution, is_subtype_of, methods_of_type, substitute_struct_fields, substitute_type_parameter, SubstitutionMap, TypeEnvironment, TypeInfo, TypeInfos, VariableEnvironment};
use std::fmt::{Debug, Write};
use std::fs::File;
use std::io::Write as IOWrite;

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

pub(crate) fn monomorph_program<'a, 'b>(program: &'a Program<'b>, type_infos: &'a TypeInfos<'b>) -> Result<String, RufegoError> {
    let instance_set = instance_set_of(&program.expression, &HashMap::new(), &HashMap::new(), type_infos)?;
    let delta = TypeEnvironment::new();

    let mut omega = instance_set;
    let mut previous_length = 0;

    loop {
        let iteration_result = g_function(&omega, &delta, type_infos)?;

        omega.extend(iteration_result);

        if previous_length == omega.len() {
            break;
        }

        previous_length = omega.len();
    }

    let mut program_code = String::new();

    writeln!(&mut program_code, "package main;\n").unwrap();
    writeln!(&mut program_code, "type Top struct {{}}\n").unwrap();

    for instance_type in &omega {
        match instance_type {
            InstanceType::Type { type_ } => {
                let mut mue = Vec::new();

                for inner_instance_type in &omega {
                    if let InstanceType::Method { type_: inner_type, method_name, instantiation } = inner_instance_type {
                        if inner_type == type_ {
                            mue.push((*method_name, instantiation));
                        }
                    }
                }

                if let GenericType::NamedType(type_name, instantiation) = type_ {
                    match type_infos.get(type_name).unwrap() {
                        TypeInfo::Struct { bound, methods, .. } => {
                            let substitution_map = generate_substitution(bound, instantiation)?;
                            let monomorphed_type = monomorph_type(type_, &substitution_map);
                            let monomorphed_type_declaration = monomorph_type_declaration(type_name, &substitution_map, &mue, type_infos)?;

                            for method_declaration in methods.values() {
                                let dummy_method = generate_dummy_method(type_name, bound, method_declaration, &substitution_map);
                                writeln!(&mut program_code, "{dummy_method}\n").unwrap();
                            }

                            write!(&mut program_code, "type {monomorphed_type} {monomorphed_type_declaration}").unwrap();
                        }
                        TypeInfo::Interface { bound, .. } => {
                            let substitution_map = generate_substitution(bound, instantiation)?;
                            let monomorphed_type = monomorph_type(type_, &substitution_map);
                            
                            let monomorphed_type_declaration = monomorph_type_declaration(type_name, &substitution_map, &mue, type_infos)?;

                            write!(&mut program_code, "type {monomorphed_type} {monomorphed_type_declaration}").unwrap();
                        }
                    }
                }
            }
            InstanceType::Method { type_, method_name, instantiation: method_instantiation } => {
                if let GenericType::NamedType(type_name, instantiation) = type_ {
                    if let TypeInfo::Struct { bound, methods, .. } = type_infos.get(type_name).unwrap() {
                        let method = methods.get(method_name).unwrap();

                        let type_substitution = generate_substitution(bound, instantiation)?;
                        let method_substitution = generate_substitution(&method.specification.bound, method_instantiation)?;
                        let theta = concat_substitutions(&type_substitution, &method_substitution);

                        let monomorphed_method = monomorph_method_declaration(type_name, bound, method, &theta)?;

                        writeln!(&mut program_code, "{monomorphed_method}\n").unwrap();
                    }
                }
            }
        }
    }

    let monomorphed_expression = monomorph_expression(&program.expression, &SubstitutionMap::new())?;

    writeln!(&mut program_code, "func main() {{").unwrap();
    writeln!(&mut program_code, "   _ = {}", monomorphed_expression.as_str()).unwrap();
    writeln!(&mut program_code, "}}").unwrap();

    let mut file = File::create("output/output.go").unwrap();
    file.write_all(program_code.as_bytes()).unwrap();

    Ok(program_code)
}

fn monomorph_type_declaration<'a, 'b>(
    type_name: &'a str,
    substitution_map: &'a SubstitutionMap<'b>,
    mue: &'a [(&'b str, &'a Vec<GenericType<'b>>)],
    type_infos: &'a TypeInfos<'b>,
) -> Result<String, RufegoError> {
    let mut type_declaration_string = String::new();

    match type_infos.get(type_name).unwrap() {
        TypeInfo::Struct { fields, .. } => {
            writeln!(&mut type_declaration_string, "struct {{").unwrap();

            for field in fields.iter() {
                let monomorphed_field_type = monomorph_type(&field.type_, substitution_map);
                writeln!(&mut type_declaration_string, "   {} {monomorphed_field_type}", field.name).unwrap();
            }

            writeln!(&mut type_declaration_string, "}}\n").unwrap();
        }
        TypeInfo::Interface { methods, .. } => {
            writeln!(&mut type_declaration_string, "interface {{").unwrap();

            for method in methods.iter() {
                for (method_name, instantiation) in mue {
                    if method_name != &method.name {
                        continue;
                    }

                    let method_substitution = generate_substitution(&method.bound, instantiation)?;
                    let theta = concat_substitutions(substitution_map, &method_substitution);

                    let monomorphed_method_name = monomorph_method_formal(method_name, &method.bound, &theta)?;
                    let monomorphed_method_signature = monomorph_method_signature(method, &theta)?;

                    writeln!(&mut type_declaration_string, "   {monomorphed_method_name}{monomorphed_method_signature}").unwrap();
                }

                let dummy_method_specification = generate_dummy_method_signature(method, substitution_map);
                writeln!(&mut type_declaration_string, "   {dummy_method_specification}").unwrap();
            }

            writeln!(&mut type_declaration_string, "}}\n").unwrap();
        }
    }

    Ok(type_declaration_string)
}

fn generate_dummy_method_signature<'a, 'b>(
    specification: &'a MethodSpecification<'b>,
    substitution_map: &'a SubstitutionMap<'b>,
) -> String {
    let mut dummy_method = String::new();
    let hash = generate_method_signature_hash(specification, substitution_map);

    write!(&mut dummy_method, "{}<{hash}>() Top", specification.name).unwrap();

    dummy_method
}

fn generate_method_signature_hash(specification: &MethodSpecification, substitution_map: &SubstitutionMap) -> u64 {
    let mut hasher = DefaultHasher::new();
    
    specification.name.hash(&mut hasher);

    for type_formal in &specification.bound {
        let substituted_type_formal = substitute_type_parameter(&type_formal.type_, substitution_map);
        substituted_type_formal.hash(&mut hasher);
    }

    for parameter in &specification.parameters {
        let substituted_parameter = substitute_type_parameter(&parameter.type_, substitution_map);
        substituted_parameter.hash(&mut hasher);
    }

    let substituted_return_type = substitute_type_parameter(&specification.return_type, substitution_map);
    substituted_return_type.hash(&mut hasher);

    hasher.finish()
}

fn generate_dummy_method(receiver_type: &str, bound: &Vec<GenericBinding>, method: &MethodDeclaration, substitution_map: &SubstitutionMap) -> String {
    let mut dummy_method_string = String::new();
    let monomorphed_receiver_type = monomorph_type_formal(receiver_type, bound, substitution_map);

    write!(&mut dummy_method_string, "func ({} {monomorphed_receiver_type}) ", method.receiver.name).unwrap();

    let dummy_method_signature = generate_dummy_method_signature(&method.specification, substitution_map);

    writeln!(&mut dummy_method_string, "{dummy_method_signature} {{").unwrap();
    writeln!(&mut dummy_method_string, "   return Top{{}}").unwrap();
    write!(&mut dummy_method_string, "}}").unwrap();

    dummy_method_string
}

fn monomorph_method_signature(
    method: &MethodSpecification,
    substitution_map: &SubstitutionMap,
) -> Result<String, RufegoError> {
    let mut method_signature_string = String::new();

    write!(&mut method_signature_string, "(").unwrap();

    for (index, parameter) in method.parameters.iter().enumerate() {
        let monomorphed_parameter_type = monomorph_type(&parameter.type_, substitution_map);
        write!(&mut method_signature_string, "{} {monomorphed_parameter_type}", parameter.name).unwrap();

        if index < method.parameters.len() - 1 {
            write!(&mut method_signature_string, ", ").unwrap();
        }
    }

    let monomorphed_return_type = monomorph_type(&method.return_type, substitution_map);

    write!(&mut method_signature_string, ") {monomorphed_return_type}").unwrap();

    Ok(method_signature_string)
}

fn monomorph_method_declaration(
    receiver_type: &str,
    receiver_bound: &Vec<GenericBinding>,
    method: &MethodDeclaration,
    theta: &SubstitutionMap,
) -> Result<String, RufegoError> {
    let mut method_string = String::new();

    let monomorphed_receiver_type = monomorph_type_formal(receiver_type, receiver_bound, theta);
    write!(&mut method_string, "func ({} {monomorphed_receiver_type}) ", method.receiver.name).unwrap();

    let monomorphed_method_name = monomorph_method_formal(method.specification.name, &method.specification.bound, theta)?;
    write!(&mut method_string, "{monomorphed_method_name}").unwrap();

    let monomorphed_method_signature = monomorph_method_signature(&method.specification, theta)?;
    writeln!(&mut method_string, "{monomorphed_method_signature} {{").unwrap();

    let monomorphed_body_expression = monomorph_expression(&method.body, theta)?;

    writeln!(&mut method_string, "   return {monomorphed_body_expression}").unwrap();
    write!(&mut method_string, "}}").unwrap();

    Ok(method_string)
}

fn monomorph_expression<'a, 'b>(
    expression: &'a Expression<'b>,
    substitution: &'a SubstitutionMap<'b>,
) -> Result<String, RufegoError> {
    match expression {
        Expression::Variable { name } => {
            Ok(String::from(*name))
        }
        Expression::MethodCall { expression, method, instantiation, parameter_expressions } => {
            let monomorphed_expression = monomorph_expression(expression, substitution)?;
            let monomorphed_method_name = monomorph_method(method, instantiation, substitution)?;
            let mut monomorphed_parameter_expressions = Vec::new();

            for parameter_expression in parameter_expressions {
                let monomorphed_parameter = monomorph_expression(parameter_expression, substitution)?;
                monomorphed_parameter_expressions.push(monomorphed_parameter);
            }

            let mut expression_string = String::new();

            write!(&mut expression_string, "{monomorphed_expression}.{monomorphed_method_name}(").unwrap();

            for (index, parameter_expression) in monomorphed_parameter_expressions.iter().enumerate() {
                write!(&mut expression_string, "{parameter_expression}").unwrap();

                if index < parameter_expressions.len() - 1 {
                    write!(&mut expression_string, ", ").unwrap();
                }
            }

            write!(&mut expression_string, ")").unwrap();

            Ok(expression_string)
        }
        Expression::StructLiteral { name, instantiation, field_expressions } => {
            let struct_type = GenericType::NamedType(name, instantiation.clone());
            let monomorphed_struct = monomorph_type(&struct_type, substitution);
            let mut monomorphed_field_expressions = Vec::new();

            for field_expression in field_expressions {
                let monomorphed_field = monomorph_expression(field_expression, substitution)?;
                monomorphed_field_expressions.push(monomorphed_field);
            }

            let mut expression_string = String::new();

            write!(&mut expression_string, "{monomorphed_struct}{{ ").unwrap();

            for (index, field_expression) in monomorphed_field_expressions.iter().enumerate() {
                write!(&mut expression_string, "{}", field_expression.as_str()).unwrap();

                if index < field_expressions.len() - 1 {
                    write!(&mut expression_string, ", ").unwrap();
                }
            }

            write!(&mut expression_string, " }}").unwrap();

            Ok(expression_string)
        }
        Expression::Select { expression, field } => {
            let monomorphed_expression = monomorph_expression(expression, substitution)?;

            let mut expression_string = String::new();

            write!(&mut expression_string, "{monomorphed_expression}.{field}").unwrap();

            Ok(expression_string)
        }
        Expression::TypeAssertion { expression, assert } => {
            let monomorphed_expression = monomorph_expression(expression, substitution)?;
            let monomorphed_assert_type = monomorph_type(assert, substitution);

            let mut expression_string = String::new();

            write!(&mut expression_string, "{monomorphed_expression}.({monomorphed_assert_type})").unwrap();

            Ok(expression_string)
        }
        Expression::Number { value } => {
            let mut expression_string = String::new();

            write!(&mut expression_string, "{value}").unwrap();

            Ok(expression_string)
        }
        Expression::BinOp { lhs, operator, rhs } => {
            let lhs_monomorphed = monomorph_expression(lhs, substitution)?;
            let rhs_monomorphed = monomorph_expression(rhs, substitution)?;

            let mut expression_string = String::new();

            write!(&mut expression_string, "{lhs_monomorphed} {} {rhs_monomorphed}", operator.as_str()).unwrap();

            Ok(expression_string)
        }
    }
}

fn monomorph_type<'a, 'b>(type_: &'a GenericType<'b>, substitution_map: &'a SubstitutionMap<'b>) -> String {
    let substituted_type = substitute_type_parameter(type_, substitution_map);

    substituted_type.close_type()
}

fn monomorph_type_formal<'a, 'b>(type_name: &'a str, type_formals: &'a Vec<GenericBinding<'b>>, substitution_map: &'a SubstitutionMap<'b>) -> String {
    let mut substituted_type_parameters = Vec::new();

    for formal_type in type_formals {
        let substituted_type_parameter = match substitution_map.get(formal_type.name) {
            None => {
                formal_type.type_.clone()
            }
            Some(entry) => {
                entry.clone()
            }
        };

        substituted_type_parameters.push(substituted_type_parameter);
    }

    let type_ = GenericType::NamedType(type_name, substituted_type_parameters);

    type_.close_type()
}

fn monomorph_method<'a, 'b>(method: &'a str, instantiation: &'a Vec<GenericType<'b>>, substitution_map: &'a SubstitutionMap<'b>) -> Result<String, RufegoError> {
    let mut method_name = String::new();
    write!(&mut method_name, "{method}<").unwrap();

    for (index, instantiated_type) in instantiation.iter().enumerate() {
        let substituted_type = substitute_type_parameter(instantiated_type, substitution_map);
        let monomorphed_type = monomorph_type(&substituted_type, substitution_map);

        write!(&mut method_name, "{monomorphed_type}").unwrap();

        if index < instantiation.len() - 1 {
            write!(&mut method_name, ",").unwrap();
        }
    }

    write!(&mut method_name, ">").unwrap();

    Ok(method_name)
}

fn monomorph_method_formal<'a, 'b>(method: &'a str, type_formals: &'a [GenericBinding<'b>], substitution_map: &'a SubstitutionMap<'b>) -> Result<String, RufegoError> {
    let mut substituted_type_parameters = Vec::new();

    for formal_type in type_formals.iter() {
        let substituted_type_parameter = match substitution_map.get(formal_type.name) {
            None => {
                formal_type.type_.clone()
            }
            Some(entry) => {
                entry.clone()
            }
        };

        substituted_type_parameters.push(substituted_type_parameter);
    }

    let method_name = monomorph_method(method, &substituted_type_parameters, substitution_map)?;

    Ok(method_name)
}

fn instance_set_of<'a, 'b>(
    expression: &'a Expression<'b>,
    variable_environment: &'a VariableEnvironment<'b>,
    type_environment: &'a TypeEnvironment<'b>,
    type_infos: &'a TypeInfos<'b>,
) -> Result<HashSet<InstanceType<'b>>, RufegoError> {
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
        Expression::Number { .. } => {
            let mut number_instance_set = HashSet::new();
            number_instance_set.insert(InstanceType::Type { type_: GenericType::NumberType });

            Ok(number_instance_set)
        }
        Expression::BinOp { lhs, rhs, .. } => {
            let mut binop_instance_set = HashSet::new();

            let lhs_instance_set = instance_set_of(lhs, variable_environment, type_environment, type_infos)?;
            let rhs_instance_set = instance_set_of(rhs, variable_environment, type_environment, type_infos)?;

            binop_instance_set.extend(lhs_instance_set);
            binop_instance_set.extend(rhs_instance_set);

            Ok(binop_instance_set)
        }
    }
}

fn g_function<'a, 'b>(
    instance_set: &'a HashSet<InstanceType<'b>>,
    delta: &'a TypeEnvironment<'b>,
    type_infos: &'a TypeInfos<'b>,
) -> Result<HashSet<InstanceType<'b>>, RufegoError> {
    let mut omega = HashSet::new();

    let f_result = f_closure(instance_set, type_infos)?;
    omega.extend(f_result);

    let m_result = m_closure(instance_set, delta, type_infos)?;
    omega.extend(m_result);

    let i_result = i_closure(instance_set, delta, type_infos)?;
    omega.extend(i_result);

    let s_result = s_closure(instance_set, delta, type_infos)?;
    omega.extend(s_result);

    Ok(omega)
}

fn f_closure<'a, 'b>(instance_set: &'a HashSet<InstanceType<'b>>, type_infos: &'a TypeInfos<'b>) -> Result<HashSet<InstanceType<'b>>, RufegoError> {
    let mut result_set = HashSet::new();

    for value in instance_set {
        if let InstanceType::Type { type_: GenericType::NamedType(name, instantiation) } = value {
            let type_info = type_infos.get(name).unwrap();

            if let TypeInfo::Struct { bound, fields, .. } = type_info {
                let substitution = generate_substitution(bound, instantiation)?;

                let substituted_fields = substitute_struct_fields(&substitution, fields);

                for field_binding in substituted_fields {
                    result_set.insert(InstanceType::Type { type_: field_binding.type_.clone() });
                }
            }
        }
    }

    Ok(result_set)
}

fn m_closure<'a, 'b>(
    instance_set: &'a HashSet<InstanceType<'b>>,
    delta: &'a TypeEnvironment<'b>,
    type_infos: &'a TypeInfos<'b>,
) -> Result<HashSet<InstanceType<'b>>, RufegoError> {
    let mut result_set = HashSet::new();

    for value in instance_set {
        if let InstanceType::Method { type_, method_name, instantiation } = value {
            let methods = methods_of_type(type_, delta, type_infos)?;

            if let Some(method_specification) = methods.iter().find(|method_specification| &method_specification.name == method_name) {
                let substitution = generate_substitution(&method_specification.bound, instantiation)?;

                for parameter in &method_specification.parameters {
                    let substituted_parameter = substitute_type_parameter(&parameter.type_, &substitution);
                    result_set.insert(InstanceType::Type { type_: substituted_parameter });
                }

                let substituted_return_type = substitute_type_parameter(&method_specification.return_type, &substitution);
                result_set.insert(InstanceType::Type { type_: substituted_return_type });
            }
        }
    }

    Ok(result_set)
}

fn i_closure<'a, 'b>(
    instance_set: &'a HashSet<InstanceType<'b>>,
    delta: &'a TypeEnvironment<'b>,
    type_infos: &'a TypeInfos<'b>,
) -> Result<HashSet<InstanceType<'b>>, RufegoError> {
    let mut result_set = HashSet::new();

    for first_value in instance_set {
        if let InstanceType::Method { type_, method_name, instantiation } = first_value {
            for second_value in instance_set {
                if let InstanceType::Type { type_: second_type } = second_value {
                    if let GenericType::NumberType = second_type {
                        break;
                    }

                    let type_info = type_infos.get(second_type.name()).unwrap();

                    if let TypeInfo::Interface { .. } = type_info {
                        if is_subtype_of(second_type, type_, delta, type_infos).is_ok() {
                            result_set.insert(InstanceType::Method {
                                type_: second_type.clone(),
                                method_name,
                                instantiation: instantiation.clone(),
                            });
                        }
                    }
                }
            }
        }
    }

    Ok(result_set)
}

fn s_closure<'a, 'b>(
    instance_set: &'a HashSet<InstanceType<'b>>,
    delta: &'a TypeEnvironment<'b>,
    type_infos: &'a TypeInfos<'b>,
) -> Result<HashSet<InstanceType<'b>>, RufegoError> {
    let mut result_set = HashSet::<InstanceType>::new();

    for first_value in instance_set {
        for second_value in instance_set {
            match (first_value, second_value) {
                (InstanceType::Method { type_: method_type, method_name, instantiation: method_instantiation }, InstanceType::Type { type_ }) => {
                    if let GenericType::NamedType(type_name, instantiation) = type_ {
                        if let TypeInfo::Struct { .. } = type_infos.get(type_name).unwrap() {
                            if is_subtype_of(type_, method_type, delta, type_infos).is_ok() {
                                let (mut parameters, substituted_expression) = body_of(
                                    type_name,
                                    instantiation,
                                    method_name,
                                    method_instantiation,
                                    type_infos,
                                ).unwrap();

                                // substitute receiver and parameter with evaluated values
                                let mut local_context = HashMap::new();

                                // receiver binding is stored at index 0 so remove it
                                let receiver_binding = parameters.remove(0);

                                // insert receiver to local variables
                                local_context.insert(receiver_binding.name, receiver_binding.type_);

                                for parameter_binding in parameters.iter() {
                                    local_context.insert(parameter_binding.name, parameter_binding.type_.clone());
                                }

                                let omega = instance_set_of(&substituted_expression, &local_context, delta, type_infos)?;

                                result_set.insert(InstanceType::Method {
                                    type_: type_.clone(),
                                    method_name,
                                    instantiation: method_instantiation.clone(),
                                });

                                result_set.extend(omega)
                            }
                        }
                    }
                } 
                _ => continue
            }
        }
    }

    Ok(result_set)
}