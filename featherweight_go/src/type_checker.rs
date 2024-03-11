use std::collections::{HashMap};
use parser::{Binding, Declaration, Expression, MethodDeclaration, MethodSpecification, Program, Type, TypeLiteral, RufegoError, TypeInfo};

pub(crate) fn build_type_infos<'a>(program: &'a Program<'a>) -> Result<HashMap<&'a str, TypeInfo<'a>>, RufegoError> {
    let mut types = HashMap::new();

    // check all type declarations
    for declaration in &program.declarations {
        if let Declaration::Type { name, literal } = declaration {
            if types.contains_key(name) {
                return Err(RufegoError { message: format!("ERROR: Type {:?} already declared", name) });
            } else {
                let type_info = match literal {
                    TypeLiteral::Struct { fields } => TypeInfo::Struct(fields, HashMap::new()),
                    TypeLiteral::Interface { methods } => TypeInfo::Interface(methods),
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
                    return Err(RufegoError { message: format!("ERROR: Can't declare method {:?} for unknown type {:?}", method.specification.name, method.receiver.type_) });
                }
                Some(TypeInfo::Interface(..)) => {
                    return Err(RufegoError { message: format!("ERROR: Can't implement interface method {:?} for interface {:?}", method.specification.name, method.receiver.type_) });
                }
                Some(TypeInfo::Struct(.., methods)) => {
                    // method already declared?
                    if methods.insert(method.specification.name, method).is_some() {
                        return Err(RufegoError { message: format!("ERROR: Duplicate declaration of method '{}' for type {:?}", method.specification.name, method.receiver.type_) });
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
pub(crate) fn check_program<'a>(program: &'a Program<'a>, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<Type<'a>, RufegoError> {
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
            - its type literal is well formed
        Method:
            - its receiver and formal parameters are distinct
            - all types are declared
            - the body is well typed in the appropriate environment
            - expression type implements the declared return type
 */
fn check_declaration<'a>(declaration: &Declaration<'a>, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), RufegoError> {
    match declaration {
        // is the type literal well formed?
        Declaration::Type { name, literal } => check_type_literal(name, literal, types)?,
        // is the method well formed?
        Declaration::Method(MethodDeclaration { receiver, specification, body }) => check_method(receiver, specification, body, types)?,
    }

    Ok(())
}

fn check_method(receiver: &Binding<&str>, specification: &MethodSpecification, body: &Expression, types: &HashMap<&str, TypeInfo>) -> Result<(), RufegoError> {
    // is the receiver type declared?
    check_type(&Type::Struct(receiver.type_), types)?;

    for (index, parameter) in specification.parameters.iter().enumerate() {
        // is the parameter type declared?
        check_type(&parameter.type_, types)?;

        // are the parameter names distinct?
        if receiver.name == parameter.name || specification.parameters.iter().skip(index + 1).any(|element| element.name == parameter.name) {
            return Err(RufegoError { message: format!("ERROR: Duplicate parameter name {:?} in method {:?}", parameter.name, specification.name) });
        }
    }

    // is the return-type declared?
    check_type(&specification.return_type, types)?;

    // build type context
    let mut context = HashMap::new();
    context.insert(receiver.name, Type::Struct(receiver.type_));

    for parameter in &specification.parameters {
        context.insert(parameter.name, parameter.type_.clone());
    }

    // evaluate type of body expression
    let expression_type = check_expression(body, &context, types)?;

    // is the body type at least a subtype of the return type?
    is_subtype_of(&expression_type, &specification.return_type, types)?;

    Ok(())
}

/*
    Judgement T ok => type literal T is well formed
        Structure:
            - all field names are distinct
            - all types declared
        Interface:
            - all its method specifications are well formed
            - all method names are unique
 */
fn check_type_literal<'a>(name: &'a str, type_literal: &TypeLiteral, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), RufegoError> {
    match type_literal {
        TypeLiteral::Struct { fields } => {
            for (index, field) in fields.iter().enumerate() {
                // is the field type declared?
                check_type(&field.type_, types)?;

                // no self recursion in structs
                if type_name(&field.type_) == name {
                    return Err(RufegoError { message: format!("ERROR: Struct {name} has self recursion on field {:?} which is forbidden", field.name) });
                }

                // are the field names distinct?
                if fields.iter().skip(index + 1).any(|element| element.name == field.name) {
                    return Err(RufegoError { message: format!("ERROR: Duplicate field name {:?} for struct {:?}", field.name, name) });
                }
            }
        }
        TypeLiteral::Interface { methods: method_specifications } => {
            for (index, method_specification) in method_specifications.iter().enumerate() {
                // is the method specification well formed?
                check_method_specification(method_specification, types)?;

                // are the method names unique?
                if method_specifications.iter().skip(index + 1).any(|element| element.name == method_specification.name) {
                    return Err(RufegoError { message: format!("ERROR: Duplicate interface method {:?} for interface {:?}", method_specification.name, name) });
                }
            }
        }
    }

    Ok(())
}

/*
    Judgement S ok => method specification S is well formed
        - all formal parameters x are distinct
        - all the types t are declared
 */
fn check_method_specification<'a>(method_specification: &MethodSpecification, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), RufegoError> {
    for (index, parameter) in method_specification.parameters.iter().enumerate() {
        // is the parameter type declared?
        check_type(&parameter.type_, types)?;

        // are the method parameters distinct?
        if method_specification.parameters.iter().skip(index + 1).any(|element| element.name == parameter.name) {
            return Err(RufegoError { message: format!("ERROR: Duplicate parameter name {:?} for method {:?}", parameter.name, method_specification.name) });
        }
    }

    // is the return type declared?
    check_type(&method_specification.return_type, types)?;

    Ok(())
}

/*
    Judgement t ok => type t is declared
*/
fn check_type<'a>(type_: &Type, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), RufegoError> {
    // is the type declared?
    match type_ {
        Type::Int => (),
        Type::Struct(name) => {
            if !types.contains_key(name) {
                return Err(RufegoError { message: format!("ERROR: Type '{name}' is undeclared") });
            }
        }
    }

    Ok(())
}

fn check_expression<'a>(expression: &Expression<'a>, context: &HashMap<&str, Type<'a>>, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<Type<'a>, RufegoError> {
    match expression {
        Expression::Variable { name } => {
            // variable known in this context?
            if let Some(var_type) = context.get(name) {
                Ok(var_type.clone())
            } else {
                Err(RufegoError { message: format!("ERROR: Variable {:?} is unknown in this context", name) })
            }
        }
        Expression::MethodCall { expression, method, parameter_expressions } => {
            // evaluate type of the body expression
            let expression_type = check_expression(expression, context, types)?;

            // typeinfo for the body expression
            let type_info = types.get(type_name(&expression_type)).expect("ERROR: Expression can't evaluate to an unknown type");

            match type_info {
                TypeInfo::Struct(.., methods) => {
                    // is the method implemented for this type?
                    match methods.get(method) {
                        None => {
                            Err(RufegoError { message: format!("ERROR: Method '{method}' isn't implemented for type {:?}", expression_type) })
                        }
                        Some(declaration) => {
                            // correct amount of parameters supplied?
                            if parameter_expressions.len() != declaration.specification.parameters.len() {
                                return Err(RufegoError { message: format!("ERROR: Method '{method}' expects {:?} parameters but {:?} parameters were supplied", declaration.specification.parameters.len(), parameter_expressions.len()) });
                            }

                            // does the types of the parameter expressions match the types of the method parameters?
                            for (index, expression) in parameter_expressions.iter().enumerate() {
                                if let Some(parameter) = declaration.specification.parameters.get(index) {
                                    // evaluate type of the supplied parameter expression
                                    let expression_type = check_expression(expression, context, types)?;

                                    // is the parameter at least a subtype of the method parameter?
                                    is_subtype_of(&expression_type, &parameter.type_, types)?;
                                }
                            }

                            // is the return type declared?
                            check_type(&declaration.specification.return_type, types)?;

                            Ok(declaration.specification.return_type.clone())
                        }
                    }
                }
                TypeInfo::Interface(methods) => {
                    // does the method exist on the interface?
                    match methods.iter().find(|method_specification| &method_specification.name == method) {
                        None => {
                            Err(RufegoError { message: format!("ERROR: Interface {:?} doesn't have a method named {:?}", expression_type, method) })
                        }
                        Some(method_specification) => {
                            // correct amount of parameters supplied?
                            if parameter_expressions.len() != method_specification.parameters.len() {
                                return Err(RufegoError { message: format!("ERROR: Method {:?} expects {:?} parameters but {:?} parameters were supplied", method, method_specification.parameters.len(), parameter_expressions.len()) });
                            }

                            for (index, expression) in parameter_expressions.iter().enumerate() {
                                if let Some(parameter) = method_specification.parameters.get(index) {
                                    // evaluate type of the supplied parameter expression
                                    let expression_type = check_expression(expression, context, types)?;

                                    // is the parameter at least a subtype of the method parameter?
                                    is_subtype_of(&expression_type, &parameter.type_, types)?;
                                }
                            }

                            // is the return type declared?
                            check_type(&method_specification.return_type, types)?;

                            Ok(method_specification.return_type.clone())
                        }
                    }
                }
            }
        }
        Expression::StructLiteral { name, field_expressions } => {
            match types.get(name) {
                None => {
                    Err(RufegoError { message: format!("ERROR: Struct literal {:?} is not declared", name) })
                }
                Some(type_info) => {
                    match type_info {
                        TypeInfo::Struct(fields, ..) => {
                            // correct amount of parameters supplied?
                            if field_expressions.len() != fields.len() {
                                return Err(RufegoError { message: format!("ERROR: Struct {:?} has {:?} fields but {:?} values were supplied", name, fields.len(), field_expressions.len()) });
                            }

                            for (index, expression) in field_expressions.iter().enumerate() {
                                if let Some(field) = fields.get(index) {
                                    // evaluate type of supplied parameter
                                    let field_type = check_expression(expression, context, types)?;

                                    is_subtype_of(&field_type, &field.type_, types)?;
                                }
                            }

                            Ok(Type::Struct(name))
                        }
                        TypeInfo::Interface(_) => {
                            Err(RufegoError { message: String::from("ERROR: An interface can't be instantiated") })
                        }
                    }
                }
            }
        }
        Expression::Select { expression, field: field_var } => {
            let type_name = match check_expression(expression, context, types)? {
                Type::Int => {
                    return Err(RufegoError { message: String::from("ERROR: Selections are only allowed on struct types") });
                }
                Type::Struct(name) => name,
            };

            let type_info = types.get(type_name).expect("Expression can't evaluate to an unknown type");

            match type_info {
                TypeInfo::Struct(fields, _) => {
                    if let Some(field) = fields.iter().find(|field| &field.name == field_var) {
                        Ok(field.type_.clone())
                    } else {
                        Err(RufegoError { message: format!("ERROR: Struct type {:?} doesn't have a field named {:?}", type_name, field_var) })
                    }
                }
                TypeInfo::Interface(_) => {
                    Err(RufegoError { message: String::from("ERROR: An interface can't be selected") })
                }
            }
        }
        Expression::TypeAssertion { expression, assert } => {
            // asserted type declared?
            check_type(assert, types)?;

            let assert_type_info = types.get(type_name(assert)).expect("Asserted type was checked beforehand");

            let expression_type = check_expression(expression, context, types)?;

            let body_type_info = types.get(type_name(&expression_type)).expect("Expression can't evaluate to an unknown type");

            match (assert_type_info, body_type_info) {
                (TypeInfo::Interface(..), TypeInfo::Struct(..)) => {
                    return Err(RufegoError { message: String::from("ERROR: Can't assert an interface type on a struct type") });
                }
                (TypeInfo::Struct(..), TypeInfo::Interface(..)) => {
                    is_subtype_of(assert, &expression_type, types)?
                }
                (TypeInfo::Interface(..), TypeInfo::Interface(..)) => {}
                (TypeInfo::Struct(..), TypeInfo::Struct(..)) => {}
            }

            Ok(assert.clone())
        }
        Expression::Number { .. } => {
            Ok(Type::Int)
        }
        Expression::BinOp { lhs, rhs, .. } => {
            let lhs_type = check_expression(lhs, context, types)?;
            let rhs_type = check_expression(rhs, context, types)?;

            match (lhs_type, rhs_type) {
                (Type::Int, Type::Int) => Ok(Type::Int),
                (Type::Struct(_), Type::Int) => {
                    Err(RufegoError { message: String::from("ERROR: Left operand of a binary operation doesn't evaluate to an integer type") })
                }
                (Type::Int, Type::Struct(_)) => {
                    Err(RufegoError { message: String::from("ERROR: Right operand of a binary operation doesn't evaluate to an integer type") })
                }
                (Type::Struct(_), Type::Struct(_)) => {
                    Err(RufegoError { message: String::from("ERROR: Both operands of a binary operation don't evaluate to an integer type") })
                }
            }
        }
    }
}

pub(crate) fn is_subtype_of<'a>(child_type: &Type, parent_type: &Type, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), RufegoError> {
    // a type is a subtype of itself
    if parent_type == child_type {
        return Ok(());
    }

    if child_type == &Type::Int {
        return Err(RufegoError { message: String::from("ERROR: An integer value cant be the child type of a struct value") });
    }

    let child_type_info = types.get(type_name(child_type)).expect("Function is only called with declared types");

    let methods = match parent_type {
        Type::Int => {
            return Err(RufegoError { message: String::from("ERROR: An integer value cant be the parent type of a struct value") });
        }
        Type::Struct(type_name) => {
            match types.get(type_name) {
                None => {
                    return Err(RufegoError { message: format!("ERROR: Type {} is undeclared", type_name) });
                }
                Some(type_info) => {
                    match type_info {
                        TypeInfo::Struct(..) => {
                            return Err(RufegoError { message: String::from("ERROR: Struct cant be the supertype of any type") });
                        }
                        TypeInfo::Interface(methods) => {
                            methods
                        }
                    }
                }
            }
        }
    };

    // are all methods of the parent implemented for the child type?
    for method in methods.iter() {
        match child_type_info.method_spec(method.name) {
            None => {
                return Err(RufegoError { message: format!("ERROR: Method {:?} of parent type {:?} is not implemented for child type {:?}", method.name, parent_type, child_type) });
            }
            Some(method_spec) => {
                match is_subtype_of(&method_spec.return_type, &method.return_type, types) {
                    Ok(_) => {}
                    Err(_) => {
                        return Err(RufegoError { message: format!("ERROR: Method {:?} of parent type {:?} has return type {:?} but return type of child implementation is {:?}", method.name, parent_type, method.return_type, method_spec.return_type) });
                    }
                }

                if method.parameters.len() != method_spec.parameters.len() {
                    return Err(RufegoError { message: format!("ERROR: Method {:?} of parent type {:?} has {:?} parameters but child implementation has {:?} parameters", method.name, parent_type, method.parameters.len(), method_spec.parameters.len()) });
                }

                for (index, method_parameter) in method.parameters.iter().enumerate() {
                    let child_method_parameter = method_spec.parameters.get(index).expect("Method parameter should be supplied");

                    match is_subtype_of(&child_method_parameter.type_, &method_parameter.type_, types) {
                        Ok(_) => {}
                        Err(_) => {
                            let error_message = format!(
                                "Method parameter {:?} of method '{}' of parent type '{:?}' has type '{:?}' but parameter type of child implementation is '{:?}'",
                                method_parameter.type_,
                                method.name,
                                parent_type,
                                method.return_type,
                                child_method_parameter
                            );
                            return Err(RufegoError { message: error_message });
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

fn type_name<'a>(type_: &'a Type) -> &'a str {
    match type_ {
        Type::Int => "int",
        Type::Struct(name) => name,
    }
}

