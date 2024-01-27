use std::collections::{HashMap};
use parser::{Declaration, Expression, GenericBinding, GenericType, MethodDeclaration, MethodSpecification, Program, TypeLiteral};

// TODO clone() loswerden
// TODO ok to use String here for errormessages?
// TODO Self recursion in struct
// TODO assert for int?
// TODO subtype for int?
// TODO formal/actual typing? (currently only formal)
// TODO typecontexts already in FG used
// TODO wegen Pascal fragen..

#[derive(Eq, PartialEq, Debug, Clone)]
pub(crate) enum Type<'a> {
    Int,
    Struct(&'a str),
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub(crate) enum TypeInfo<'a> {
    // bound, fields, methods
    Struct(&'a Vec<GenericBinding<'a>>, &'a Vec<GenericBinding<'a>>, HashMap<&'a str, &'a MethodDeclaration<'a>>),
    // bound, methods
    Interface(&'a Vec<GenericBinding<'a>>, &'a Vec<MethodSpecification<'a>>),
}

#[derive(Debug)]
pub(crate) struct TypeError {
    pub(crate) message: String,
}

impl<'a> TypeInfo<'a> {
    fn method_spec(&self, method_name: &'a str) -> Option<&'a MethodSpecification<'a>> {
        match self {
            TypeInfo::Struct(.., methods) => {
                methods.get(method_name).map(|method| &method.specification)
            }
            TypeInfo::Interface(.., methods) => {
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
                    TypeLiteral::Struct { fields } => TypeInfo::Struct(bound, fields, HashMap::new()),
                    TypeLiteral::Interface { methods } => TypeInfo::Interface(bound, methods),
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
                Some(TypeInfo::Interface(..)) => {
                    return Err(TypeError { message: format!("ERROR: Can't implement interface method {:?} for interface {:?}", method.specification.name, method.receiver.type_) });
                }
                Some(TypeInfo::Struct(.., methods)) => {
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
pub(crate) fn check_program<'a>(program: &'a Program<'a>, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<Type<'a>, TypeError> {
    // are the declarations well formed?
    for declaration in &program.declarations {
        check_declaration(declaration, types)?;
    }

    Ok(Type::Int)

    // is the body well formed in the empty context?
    // check_expression(&program.expression, &HashMap::new(), types)
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
fn check_declaration<'a>(declaration: &Declaration<'a>, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), TypeError> {
    match declaration {
        Declaration::Type { name, bound, literal } => {
            check_type_literal(name, bound, literal, types)?;
        }
        Declaration::Method(MethodDeclaration { .. }) => {

        }
    }

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
fn check_type_literal<'a>(name: &'a str, bound: &[GenericBinding<'a>], type_literal: &TypeLiteral, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), TypeError> {
    match type_literal {
        TypeLiteral::Struct { fields } => {
            // create environment
            let mut context = HashMap::new();

            for binding in bound {
                context.insert(binding.name, &binding.type_);
            }

            for (index, field) in fields.iter().enumerate() {
                // field type declared?
                check_type(&field.type_, &context, types)?;

                // are the field names distinct?
                if fields.iter().skip(index + 1).any(|element| element.name == field.name) {
                    return Err(TypeError { message: format!("ERROR: Duplicate field name {:?} for struct {:?}", field.name, name) });
                }
            }
        }
        TypeLiteral::Interface { .. } => {

        }
    }

    Ok(())
}

/*
    Judgement S ok => method specification S is well formed
        - all formal parameters x are distinct
        - all the types t are declared
 */
fn check_method_specification<'a>(method_specification: &MethodSpecification, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), TypeError> {

    Ok(())
}

/*
    Judgement t ok => type is well formed
        - all type parameters in it must be declared
        - all named types must be instantiated with type arguments
*/
fn check_type<'a>(type_: &GenericType<'a>, context: &HashMap<&'a str, &GenericType<'a>>, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), TypeError> {
    // T => generic parameter
    // Consumer(int) / Consumer(Client()) / Consumer(T) => named type ??

    match type_ {
        GenericType::TypeParameter(type_parameter) => {
            if !context.contains_key(type_parameter) {
                return Err(TypeError { message: format!("ERROR: Usage of unknown generic parameter {}", type_parameter) });
            }
        }
        GenericType::NamedType(name, instantiation) => {
            if !types.contains_key(name) {
                return Err(TypeError { message: format!("ERROR: Type {} is undeclared", name) });
            }

            for parameter in instantiation {
                // type declared?
                check_type(parameter, context, types)?;
            }

            // bounds satisfied?
            check_type_bound(name, instantiation, context, types)?;
        }
        GenericType::NumberType => ()
    }

    Ok(())
}

fn check_type_bound<'a>(type_: &'a str, instantiation: &Vec<GenericType<'a>>, context: &HashMap<&'a str, &GenericType<'a>>, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), TypeError> {
    let type_info = types.get(type_).expect("Occurence of type was checked beforehand");

    match type_info {
        TypeInfo::Struct(bound, ..) => {
            // correct amount of generic parameters supplied?
            if bound.len() != instantiation.len() {
                return Err(TypeError { message: format!("ERROR: Type {} has {} generic parameters but {} parameters were supplied", type_, bound.len(), instantiation.len()) });
            }

            for parameter in bound.iter() {
                match &parameter.type_ {
                    GenericType::TypeParameter(type_parameter) => {
                        if !context.contains_key(type_parameter) {
                            return Err(TypeError { message: format!("ERROR: Usage of unknown generic parameter {}", type_parameter) });
                        }
                    }
                    GenericType::NamedType(nested_type, nested_instantiation) => {
                        check_type_bound(nested_type, nested_instantiation, context, types)?;
                    }
                    GenericType::NumberType => ()
                }
            }
        }
        TypeInfo::Interface(_, _) => {}
    }

    Ok(())
}

fn check_expression<'a>(expression: &Expression<'a>, context: &HashMap<&str, Type<'a>>, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<Type<'a>, TypeError> {
    match expression {
        Expression::Variable { .. } => {}
        Expression::MethodCall { .. } => {}
        Expression::StructLiteral { .. } => {}
        Expression::Select { .. } => {}
        Expression::TypeAssertion { .. } => {}
        Expression::Number { .. } => {}
        Expression::BinOp { .. } => {}
    }

    todo!()
}

pub(crate) fn is_subtype_of<'a>(child_type: &GenericType, parent_type: &GenericType, context: &HashMap<&str, Type<'a>>, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), TypeError> {

    Ok(())
}

