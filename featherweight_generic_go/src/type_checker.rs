use std::collections::{HashMap};
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

#[derive(Debug)]
pub(crate) struct TypeError {
    pub(crate) message: String,
}

impl<'a> TypeInfo<'a> {
    fn method_spec(&self, method_name: &'a str) -> Option<&'a MethodSpecification<'a>> {
        match self {
            TypeInfo::Struct { methods, .. } => {
                methods.get(method_name).map(|method| &method.specification)
            }
            TypeInfo::Interface { methods, .. } => {
                methods.iter().find(|method| method.name == method_name)
            }
        }
    }
}

pub(crate) fn build_type_infos<'a>(program: &'a Program<'a>) -> Result<HashMap<&'a str, TypeInfo<'a>>, TypeError> {
    let mut types = HashMap::new();

    // collect type infos for type declarations
    for declaration in &program.declarations {
        if let Declaration::Type { name, bound, literal } = declaration {
            if types.contains_key(name) {
                return Err(TypeError { message: format!("ERROR: Type {:?} already declared", name) });
            } else {
                let type_info = match literal {
                    TypeLiteral::Struct { fields } => TypeInfo::Struct { bound, fields, methods: HashMap::new() },
                    TypeLiteral::Interface { methods } => TypeInfo::Interface { bound, methods },
                };

                types.insert(*name, type_info);
            }
        }
    }

    // check all method declarations
    for declaration in &program.declarations {
        if let Declaration::Method(method) = declaration {
            match types.get_mut(method.receiver.type_) {
                None => {
                    return Err(TypeError { message: format!("ERROR: Can't declare method {:?} for unknown type {:?}", method.specification.name, method.receiver.type_) });
                }
                Some(TypeInfo::Interface { .. }) => {
                    return Err(TypeError { message: format!("ERROR: Can't implement interface method {:?} for interface {:?}", method.specification.name, method.receiver.type_) });
                }
                Some(TypeInfo::Struct { methods, .. }) => {
                    // method already declared?
                    if methods.insert(method.specification.name, method).is_some() {
                        return Err(TypeError { message: format!("ERROR: Duplicate declaration for method {:?} on type {:?}", method.specification.name, method.receiver.type_) });
                    }
                }
            }
        }
    }

    Ok(types)
}

/*
    Judgement P ok => program P is well formed
        - all type declarations are distinct
        - all method declarations are distinct
        - body well formed in the empty context
 */
pub(crate) fn check_program<'a>(program: &'a Program<'a>, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<GenericType<'a>, TypeError> {
    // are the declarations well formed?
    for declaration in &program.declarations {
        check_declaration(declaration, types)?;
    }

    // is the body well formed in the empty context?
    check_expression(&program.expression, &HashMap::new(), types)
}

/*
    Judgement D ok => declaration D is well formed
        Type literal:
            - type formals must be well formed in the empty type environment
            - its type literal is well formed in the environment given by the type formals
        Method:
            - its receiver and formal parameters are distinct
            - all types are declared
            - the body is well typed in the appropriate environment
            - expression type implements the declared return type
 */
fn check_declaration<'a>(declaration: &Declaration<'a>, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), TypeError> {
    match declaration {
        Declaration::Type { name, bound, literal } => check_type_literal(name, bound, literal, types)?,
        Declaration::Method(MethodDeclaration { receiver, specification, body }) => check_method(receiver, specification, body, types)?,
    }

    Ok(())
}

/*
    Judgement T ok => type literal T is well formed under the type formals
        Structure:
            - all field names are distinct
            - all types declared
        Interface:
            - all its method specifications are well formed
            - all method names are unique
 */
fn check_type_literal<'a>(name: &'a str, bound: &[GenericBinding<'a>], type_literal: &TypeLiteral, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), TypeError> {
    // create type formal environment
    let mut environment = HashMap::new();

    // type formals well formed in the empty type environment?
    for binding in bound {
        match types.get(type_name(&binding.type_)) {
            None => {
                return Err(TypeError { message: format!("ERROR: Use of undeclared type formal '{:?}'", binding.type_) });
            }
            Some(type_info) => {
                match type_info {
                    TypeInfo::Struct { .. } => return Err(TypeError { message: String::from("ERROR: Only interface types can be a bound") }),
                    TypeInfo::Interface { .. } => {
                        environment.insert(binding.name, binding.type_.clone());
                    }
                }
            }
        }
    }

    match type_literal {
        TypeLiteral::Struct { fields } => {
            for (index, field) in fields.iter().enumerate() {
                // field type declared?
                check_type(&field.type_, &environment, types)?;

                // are the field names distinct?
                if fields.iter().skip(index + 1).any(|element| element.name == field.name) {
                    return Err(TypeError { message: format!("ERROR: Duplicate field name {:?} for struct {:?}", field.name, name) });
                }
            }
        }
        TypeLiteral::Interface { methods } => {
            for (index, method_specification) in methods.iter().enumerate() {
                // is the method specification well formed?
                check_method_specification(method_specification, &environment, types)?;

                // are the method names unique?
                if methods.iter().skip(index + 1).any(|element| element.name == method_specification.name) {
                    return Err(TypeError { message: format!("ERROR: Duplicate interface method '{}' for interface '{name}'", method_specification.name) });
                }
            }
        }
    }

    Ok(())
}

/*
    Judgement Φ; Ψ ok => method declaration is well formed
        - Φ is well formed under the empty type environment
        - Ψ is well formed under Φ
        - receiver type and parameter types are distinct
        - type of body expression is subtype of return type
 */
fn check_method<'a>(receiver: &GenericReceiver, specification: &MethodSpecification, body: &Expression, types: &HashMap<&str, TypeInfo>) -> Result<(), TypeError> {
    // keep track of bound types
    let mut instantiated_types = Vec::new();

    // create type formal environment
    let mut receiver_environment = HashMap::new();

    // type formals well formed in the empty type environment?
    for binding in &receiver.instantiation {
        match types.get(type_name(&binding.type_)) {
            None => {
                return Err(TypeError { message: format!("ERROR: Use of undeclared type formal '{:?}'", binding.type_) });
            }
            Some(type_info) => {
                match type_info {
                    TypeInfo::Struct { .. } => return Err(TypeError { message: String::from("ERROR: Only interface types can be a bound") }),
                    TypeInfo::Interface { .. } => {
                        receiver_environment.insert(binding.name, binding.type_.clone());
                    }
                }
            }
        }

        instantiated_types.push(binding.type_.clone());
    }

    // create receiver type
    let receiver_type = GenericType::NamedType(receiver.type_, instantiated_types);

    // receiver type well formed under the receiver type environment?
    check_type(&receiver_type, &receiver_environment, types)?;

    for binding in &receiver.instantiation {
        check_type(&binding.type_, &receiver_environment, types)?;
    }

    // create environment for method type formals
    let mut method_environment = HashMap::new();

    // are the type formals of the method distinct to the type formals of the receiver?
    if specification.bound.iter().any(|element| receiver_environment.get(element.name).is_some()) {
        return Err(TypeError { message: format!("ERROR: Duplicate type formal in method '{}'", specification.name) });
    }

    for binding in &specification.bound {
        // Ψ is well formed under Φ ?
        check_type(&binding.type_, &receiver_environment, types)?;

        let type_info = match types.get(type_name(&binding.type_)) {
            None => {
                // if type is not found, check environment of receiver
                match receiver_environment.get(type_name(&binding.type_)) {
                    None => {
                        return Err(TypeError { message: format!("ERROR: Use of undeclared type formal '{:?}'", binding.type_) });
                    }
                    Some(type_) => {
                        types.get(type_name(type_)).expect("Type should be present in types")
                    }
                }
            }
            Some(type_info) => type_info,
        };

        match type_info {
            TypeInfo::Struct { .. } => return Err(TypeError { message: String::from("ERROR: Only interface types can be a bound") }),
            TypeInfo::Interface { .. } => {
                method_environment.insert(binding.name, binding.type_.clone());
            }
        }
    }

    // concatenate both type environments
    let environment = method_environment.into_iter().chain(receiver_environment).collect();

    for (index, parameter) in specification.parameters.iter().enumerate() {
        // parameter type well formed?
        check_type(&parameter.type_, &environment, types)?;

        // are the parameter names distinct?
        if receiver.name == parameter.name || specification.parameters.iter().skip(index + 1).any(|element| element.name == parameter.name) {
            return Err(TypeError { message: format!("ERROR: Duplicate parameter name {:?} in method {:?}", parameter.name, specification.name) });
        }
    }

    // return-type well formed?
    check_type(&specification.return_type, &environment, types)?;

    // build type context
    let mut context = HashMap::new();

    context.insert(receiver.name, receiver_type);

    for parameter in &specification.parameters {
        context.insert(parameter.name, parameter.type_.clone());
    }

    // evaluate type of body expression
    let expression_type = check_expression(body, &context, types)?;

    // is the body type a subtype of the return type?
    is_subtype_of(&expression_type, &specification.return_type, &environment, types)?;

    Ok(())
}

/*
    Judgement S ok => method specification S is well formed
        - all formal parameters x are distinct
        - all the types t are well formed
 */
fn check_method_specification<'a>(method_specification: &MethodSpecification, environment: &HashMap<&'a str, GenericType<'a>>, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), TypeError> {
    for (index, parameter) in method_specification.parameters.iter().enumerate() {
        // is the parameter type well formed?
        check_type(&parameter.type_, environment, types)?;

        // are the method parameters distinct?
        if method_specification.parameters.iter().skip(index + 1).any(|element| element.name == parameter.name) {
            return Err(TypeError { message: format!("ERROR: Duplicate parameter name {:?} for method {:?}", parameter.name, method_specification.name) });
        }
    }

    // is the return type well formed?
    check_type(&method_specification.return_type, environment, types)?;

    Ok(())
}

/*
    Judgement t ok => type is well formed
        - all type parameters in it must be declared
        - all named types must be instantiated with type arguments
*/
fn check_type<'a>(type_: &GenericType<'a>, environment: &HashMap<&'a str, GenericType<'a>>, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), TypeError> {
    // T => generic parameter
    // Consumer(int) / Consumer(Client()) / Consumer(T) => named type ??
    // TODO correct instantiation of bounds..
    match type_ {
        GenericType::TypeParameter(type_parameter) => {
            if !environment.contains_key(type_parameter) {
                return Err(TypeError { message: format!("ERROR: Usage of unknown generic parameter {}", type_parameter) });
            }
        }
        GenericType::NamedType(name, instantiation) => {
            if !types.contains_key(name) {
                return Err(TypeError { message: format!("ERROR: Type {} is undeclared", name) });
            }

            for parameter in instantiation {
                // type declared?
                check_type(parameter, environment, types)?;
            }

            // bounds satisfied?
            check_type_bound(name, instantiation, environment, types)?;
        }
        GenericType::NumberType => (),
    }

    Ok(())
}

fn check_type_bound<'a>(type_: &'a str, instantiation: &Vec<GenericType<'a>>, environment: &HashMap<&'a str, GenericType<'a>>, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), TypeError> {
    let type_info = types.get(type_).expect("Occurrence of type was checked beforehand");

    match type_info {
        TypeInfo::Struct { bound, .. } => {
            // correct amount of generic parameters supplied?
            if bound.len() != instantiation.len() {
                return Err(TypeError { message: format!("ERROR: Type '{type_}' has {} generic parameters but {} parameters were supplied", bound.len(), instantiation.len()) });
            }

            for (index, parameter) in bound.iter().enumerate() {
                match &parameter.type_ {
                    GenericType::TypeParameter(type_parameter) => {
                        if !environment.contains_key(type_parameter) {
                            return Err(TypeError { message: format!("ERROR: Usage of unknown generic parameter '{type_parameter}'") });
                        }
                    }
                    GenericType::NamedType(nested_type, nested_instantiation) => {
                        let instantiated_type = instantiation.get(index).expect("Vectors are of the same length");

                        // check if instantiated type is at least subtype of corresponding type
                        is_subtype_of(instantiated_type, &parameter.type_, environment, types)?;

                        // check nested parameter
                        check_type_bound(nested_type, nested_instantiation, environment, types)?;
                    }
                    GenericType::NumberType => ()
                }
            }
        }
        TypeInfo::Interface { .. } => {}
    }

    Ok(())
}

fn check_expression<'a>(expression: &Expression<'a>, environment: &HashMap<&str, GenericType<'a>>, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<GenericType<'a>, TypeError> {
    match expression {
        Expression::Variable { name } => {
            // variable known in this context?
            if let Some(var_type) = environment.get(name) {
                Ok(var_type.clone())
            } else {
                Err(TypeError { message: format!("ERROR: Variable '{name}' is unknown in this context") })
            }
        }
        Expression::MethodCall { .. } => {
            todo!()
        }
        Expression::StructLiteral { name, field_expressions, .. } => {
            // TODO check for bound missing..
            match types.get(name) {
                None => {
                    Err(TypeError { message: format!("ERROR: Struct literal {:?} is not declared", name) })
                }
                Some(type_info) => {
                    match type_info {
                        TypeInfo::Struct { fields, .. } => {
                            // correct amount of parameters supplied?
                            if field_expressions.len() != fields.len() {
                                return Err(TypeError { message: format!("ERROR: Struct {:?} has {:?} fields but {:?} values were supplied", name, fields.len(), field_expressions.len()) });
                            }

                            for (index, expression) in field_expressions.iter().enumerate() {
                                if let Some(field) = fields.get(index) {
                                    // evaluate type of supplied parameter
                                    let field_type = check_expression(expression, environment, types)?;

                                    is_subtype_of(&field_type, &field.type_, environment, types)?;
                                }
                            }

                            Ok(GenericType::NamedType(name, Vec::new()))
                        }
                        TypeInfo::Interface { .. } => {
                            Err(TypeError { message: String::from("ERROR: An interface can't be instantiated") })
                        }
                    }
                }
            }
        }
        Expression::Select { expression, field: field_var } => {
            let type_name = match check_expression(expression, environment, types)? {
                GenericType::NumberType => {
                    return Err(TypeError { message: String::from("ERROR: Selections are only allowed on struct types") });
                }
                GenericType::TypeParameter(_) => {
                    return Err(TypeError { message: String::from("ERROR: Expression of a structure literal evaluated to a type parameter") });
                }
                GenericType::NamedType(name, _) => name,
            };

            let type_info = types.get(type_name).expect("Expression can't evaluate to an unknown type");

            match type_info {
                TypeInfo::Struct { fields, .. } => {
                    if let Some(field) = fields.iter().find(|field| &field.name == field_var) {
                        Ok(field.type_.clone())
                    } else {
                        Err(TypeError { message: format!("ERROR: Struct type {:?} doesn't have a field named {:?}", type_name, field_var) })
                    }
                }
                TypeInfo::Interface { .. } => {
                    Err(TypeError { message: String::from("ERROR: An interface can't be selected") })
                }
            }
        }
        Expression::TypeAssertion { .. } => {
            todo!()
        }
        Expression::Number { .. } => {
            Ok(GenericType::NumberType)
        }
        Expression::BinOp { lhs, rhs, .. } => {
            let lhs_type = check_expression(lhs, environment, types)?;
            let rhs_type = check_expression(rhs, environment, types)?;

            match (lhs_type, rhs_type) {
                (GenericType::NumberType, GenericType::NumberType) => Ok(GenericType::NumberType),
                (_, GenericType::NumberType) => {
                    Err(TypeError { message: String::from("ERROR: Left operand of a binary operation doesn't evaluate to an integer type") })
                }
                (GenericType::NumberType, _) => {
                    Err(TypeError { message: String::from("ERROR: Right operand of a binary operation doesn't evaluate to an integer type") })
                }
                (_, _) => {
                    Err(TypeError { message: String::from("ERROR: Both operands of a binary operation don't evaluate to an integer type") })
                }
            }
        }
    }
}

pub(crate) fn is_subtype_of<'a>(child_type: &GenericType, parent_type: &GenericType, environment: &HashMap<&str, GenericType<'a>>, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), TypeError> {
    // a type is a subtype of itself
    if parent_type == child_type {
        return Ok(());
    }

    if child_type == &GenericType::NumberType {
        return Err(TypeError { message: String::from("ERROR: An integer value cant be the child type of a struct value") });
    }

    let child_type_info = match types.get(type_name(child_type)) {
        None => {
            // if type is not found, check environment of receiver
            match environment.get(type_name(child_type)) {
                None => {
                    return Err(TypeError { message: format!("ERROR: Use of undeclared type formal '{:?}'", child_type) });
                }
                Some(type_) => {
                    types.get(type_name(type_)).expect("Type should be present in types")
                }
            }
        }
        Some(type_info) => type_info,
    };

    Ok(())
}

fn type_name<'a>(type_: &'a GenericType) -> &'a str {
    match type_ {
        GenericType::TypeParameter(type_parameter) => type_parameter,
        GenericType::NamedType(name, _) => name,
        GenericType::NumberType => "int",
    }
}

