use std::collections::{HashMap};
use parser::{Declaration, Expression, GenericBinding, MethodDeclaration, MethodSpecification, Program, TypeLiteral};

// TODO clone() loswerden
// TODO ok to use String here?
// TODO Self recursion in struct
// TODO formal/actual typing? (currently only formal)

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
fn check_declaration<'a>(declaration: &Declaration<'a>, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), TypeError> {
    match declaration {
        Declaration::Type { .. } => {

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
fn check_type_literal<'a>(name: &'a str, type_literal: &TypeLiteral, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), TypeError> {
    match type_literal {
        TypeLiteral::Struct { .. } => {

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
fn check_type<'a>(type_: &Type, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), TypeError> {
    // T => generic parameter
    // Consumer(int) / Consumer(Client()) / Consumer(T) => named type ??

    // all generic type parameters declared?

    // named types instantiated with satisfied bounds

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

pub(crate) fn is_subtype_of<'a>(child_type: &Type, parent_type: &Type, types: &HashMap<&'a str, TypeInfo<'a>>) -> Result<(), TypeError> {

    Ok(())
}

