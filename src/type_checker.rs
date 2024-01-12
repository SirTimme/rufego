use std::collections::{HashMap};
use std::process::exit;
use parser::{Binding, Declaration, Expression, MethodDeclaration, MethodSpecification, Program, TypeLiteral};

// TODO method call from interface?
// TODO clone() loswerden

pub(crate) struct TypeChecker<'a> {
    pub(crate) program: &'a Program<'a>,
    pub(crate) types: HashMap<&'a str, TypeInfo<'a>>,
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub(crate) enum Type<'a> {
    Int,
    Struct(&'a str),
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub(crate) enum TypeInfo<'a> {
    Struct(&'a Vec<Binding<'a, Type<'a>>>, HashMap<&'a str, &'a MethodDeclaration<'a>>),
    Interface(&'a Vec<MethodSpecification<'a>>),
}

impl<'a> TypeInfo<'a> {
    pub(crate) fn method_spec(&self, method_name: &'a str) -> Option<&'a MethodSpecification<'a>> {
        match self {
            TypeInfo::Struct(.., methods) => {
                methods.get(method_name).map(|method| &method.specification)
            }
            TypeInfo::Interface(methods) => {
                methods.iter().find(|method| method.name == method_name)
            }
        }
    }
}

impl TypeChecker<'_> {
    pub(crate) fn type_check(&mut self) {
        for declaration in &self.program.declarations {
            if let Declaration::Type { literal } = declaration {
                if self.types.contains_key(literal.name()) {
                    eprintln!("ERROR: Type {:?} already declared", literal.name());
                    exit(1);
                } else {
                    let type_info = match literal {
                        TypeLiteral::Struct { fields, .. } => {
                            TypeInfo::Struct(fields, HashMap::new())
                        }
                        TypeLiteral::Interface { methods, .. } => {
                            TypeInfo::Interface(methods)
                        }
                    };

                    self.types.insert(literal.name(), type_info);
                }
            }
        }

        for declaration in &self.program.declarations {
            if let Declaration::Method(method) = declaration {
                match self.types.get_mut(method.receiver.type_) {
                    None => {
                        eprintln!("ERROR: Can't declare method {:?} for unknown type {:?}", method.specification.name, method.receiver.type_);
                        exit(1);
                    }
                    Some(TypeInfo::Interface(..)) => {
                        eprintln!("ERROR: Can't implement interface method {:?} for interface {:?}", method.specification.name, method.receiver.type_);
                        exit(1);
                    }
                    Some(TypeInfo::Struct(.., methods)) => {
                        if methods.insert(method.specification.name, method).is_some() {
                            eprintln!("ERROR: Duplicate declaration for method {:?} on type {:?}", method.specification.name, method.receiver.type_);
                            exit(1);
                        }
                    }
                }
            }
        }

        self.check_program();
    }

    /*
        Judgement P ok => program P is well formed
            - all type declarations are distinct
            - all method declarations are distinct
            - body well formed in the empty context
     */
    fn check_program(&mut self) {
        // declarations well formed?
        for declaration in &self.program.declarations {
            self.check_declaration(declaration);
        }

        // body well formed in the empty context?
        _ = self.check_expression(&self.program.expression, &HashMap::new());
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
    fn check_declaration(&self, declaration: &Declaration) {
        match declaration {
            Declaration::Type { literal } => {
                self.check_type_literal(literal);
            }
            Declaration::Method(MethodDeclaration { receiver, specification, body }) => {
                // receiver type declared?
                self.check_type(&Type::Struct(receiver.type_));

                for (index, parameter) in specification.parameters.iter().enumerate() {
                    // parameter type declared?
                    self.check_type(&parameter.type_);

                    // parameter names distinct?
                    if receiver.name == parameter.name || specification.parameters.iter().skip(index + 1).any(|element| element.name == parameter.name) {
                        eprintln!("ERROR: Duplicate parameter name {:?} in method {:?}", parameter.name, specification.name);
                        exit(1);
                    }
                }

                // return-type declared?
                self.check_type(&specification.return_type);

                // build type context
                let mut context = HashMap::new();
                context.insert(receiver.name, Type::Struct(receiver.type_));

                for parameter in &specification.parameters {
                    context.insert(parameter.name, parameter.type_.clone());
                }

                // determine type of body
                let expression_type = self.check_expression(body, &context);

                // body type implement return type?
                if !self.is_subtype_of(&specification.return_type, &expression_type) {
                    eprintln!("ERROR: Body expression of method {:?} evaluates to type {:?} which isn't a subtype of {:?}", specification.name, expression_type, specification.return_type, );
                    exit(1);
                }
            }
        }
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
    fn check_type_literal(&self, type_literal: &TypeLiteral) {
        match type_literal {
            TypeLiteral::Struct { name, fields } => {
                for (index, field) in fields.iter().enumerate() {
                    // field type declared?
                    self.check_type(&field.type_);

                    // field names distinct?
                    if fields.iter().skip(index + 1).any(|element| element.name == field.name) {
                        eprintln!("ERROR: Duplicate field name {:?} for struct {:?}", field.name, name);
                        exit(1);
                    }
                }
            }
            TypeLiteral::Interface { name, methods: method_specifications } => {
                for (index, method_specification) in method_specifications.iter().enumerate() {
                    // method specification well formed?
                    self.check_method_specification(method_specification);

                    // method name unique?
                    if method_specifications.iter().skip(index + 1).any(|element| element.name == method_specification.name) {
                        eprintln!("ERROR: Duplicate interface method {:?} for interface {:?}", method_specification.name, name);
                        exit(1);
                    }
                }
            }
        }
    }

    /*
        Judgement S ok => method specification S is well formed
            - all formal parameters x are distinct
            - all the types t are declared
     */
    fn check_method_specification(&self, method_specification: &MethodSpecification) {
        for (index, parameter) in method_specification.parameters.iter().enumerate() {
            // parameter type declared?
            self.check_type(&parameter.type_);

            // method parameters distinct?
            if method_specification.parameters.iter().skip(index + 1).any(|element| element.name == parameter.name) {
                eprintln!("ERROR: Duplicate parameter name {:?} for method {:?}", parameter.name, method_specification.name);
                exit(1);
            }
        }

        // return types declared?
        self.check_type(&method_specification.return_type);
    }

    /*
        Judgement t ok => type t is declared
    */
    fn check_type(&self, type_: &Type) {
        // type declared?
        match type_ {
            Type::Int => (),
            Type::Struct(name) => {
                if !self.types.contains_key(name) {
                    eprintln!("ERROR: Type {:?} is undeclared", name);
                    exit(1);
                }
            }
        }
    }

    fn check_expression<'a>(&'a self, expression: &Expression<'a>, context: &HashMap<&str, Type<'a>>) -> Type<'a> {
        match expression {
            Expression::Variable { name } => {
                if let Some(var_type) = context.get(name) {
                    // TODO w/o clone?
                    var_type.clone()
                } else {
                    eprintln!("ERROR: Variable {:?} is unknown in this context", name);
                    exit(1);
                }
            }
            Expression::MethodCall { expression, method, parameter_expressions } => {
                let type_name = match self.check_expression(expression, context) {
                    Type::Int => {
                        eprintln!("ERROR: Body of method call {:?} evaluates to an integer", method);
                        exit(1);
                    }
                    Type::Struct(name) => name
                };

                match self.types.get(type_name) {
                    None => {
                        eprintln!("ERROR: Body of method call {:?} evaluates to an unknown type", method);
                        exit(1);
                    }
                    Some(type_info) => {
                        match type_info {
                            TypeInfo::Struct(.., methods) => {
                                match methods.get(method) {
                                    None => {
                                        eprintln!("ERROR: Method {:?} isn't implemented for type {:?}", method, type_name);
                                        exit(1);
                                    }
                                    Some(declaration) => {
                                        if parameter_expressions.len() != declaration.specification.parameters.len() {
                                            eprintln!("ERROR: Method {:?} expects {:?} parameters but {:?} parameters were supplied", method, declaration.specification.parameters.len(), parameter_expressions.len());
                                            exit(1);
                                        }

                                        for (index, expression) in parameter_expressions.iter().enumerate() {
                                            if let Some(parameter) = declaration.specification.parameters.get(index) {
                                                // evaluate type of supplied parameter
                                                let expression_type = self.check_expression(expression, context);

                                                if !self.is_subtype_of(&parameter.type_, &expression_type) {
                                                    eprintln!("ERROR: Method parameter {:?} of method {:? } has type {:?} but type {:?} was supplied", parameter.name, declaration.specification.name, parameter.type_, expression_type);
                                                    exit(1);
                                                }
                                            }
                                        }

                                        // return type declared?
                                        self.check_type(&declaration.specification.return_type);

                                        // TODO w/o clone?
                                        declaration.specification.return_type.clone()
                                    }
                                }
                            }
                            TypeInfo::Interface(_) => {
                                eprintln!("ERROR: Method cant be called from an interface");
                                exit(1);
                            }
                        }
                    }
                }
            }
            Expression::StructLiteral { name, field_expressions } => {
                match self.types.get(name) {
                    None => {
                        eprintln!("ERROR: Struct literal {:?} is not declared", name);
                        exit(1);
                    }
                    Some(type_info) => {
                        match type_info {
                            TypeInfo::Struct(fields, ..) => {
                                // correct amount of parameters supplied?
                                if field_expressions.len() != fields.len() {
                                    eprintln!("ERROR: Struct {:?} has {:?} fields but {:?} values were supplied", name, fields.len(), field_expressions.len());
                                    exit(1);
                                }

                                for (index, expression) in field_expressions.iter().enumerate() {
                                    if let Some(field) = fields.get(index) {
                                        // evaluate type of supplied parameter
                                        let field_type = self.check_expression(expression, context);

                                        if !self.is_subtype_of(&field.type_, &field_type) {
                                            eprintln!("ERROR: Field {:?} of type {:?} has type {:?} but type {:?} was supplied", field.name, name, field.type_, field_type);
                                            exit(1);
                                        }
                                    }
                                }

                                Type::Struct(name)
                            }
                            TypeInfo::Interface(_) => {
                                eprintln!("ERROR: An interface can't be instantiated");
                                exit(1);
                            }
                        }
                    }
                }
            }
            Expression::Select { expression, field: field_var } => {
                let type_name = match self.check_expression(expression, context) {
                    Type::Int => {
                        eprintln!("ERROR: Selections are only allowed on struct types");
                        exit(1);
                    }
                    Type::Struct(name) => name,
                };

                match self.types.get(type_name) {
                    None => {
                        eprintln!("ERROR: Type {:?} is undeclared", type_name);
                        exit(1);
                    }
                    Some(type_info) => {
                        match type_info {
                            TypeInfo::Struct(fields, _) => {
                                if let Some(field) = fields.iter().find(|field| &field.name == field_var) {
                                    field.type_.clone()
                                } else {
                                    eprintln!("ERROR: Struct type {:?} doesn't have a field named {:?}", type_name, field_var);
                                    exit(1);
                                }
                            }
                            TypeInfo::Interface(_) => {
                                eprintln!("ERROR: An interface can't be selected");
                                exit(1);
                            }
                        }
                    }
                }
            }
            Expression::TypeAssertion { expression, assert } => {
                // asserted type declared?
                self.check_type(assert);

                // typeinfo for asserted type
                let assert_type_info = self.types.get(self.type_name(assert)).expect("Should always find typeinfo since the presence of the type was checked beforehand");

                // expression type of the body
                let expression_type = self.check_expression(expression, context);

                let body_type_info = match self.types.get(self.type_name(&expression_type)) {
                    None => {
                        eprintln!("ERROR: Type {:?} is undeclared", expression_type);
                        exit(1);
                    }
                    Some(body_type_info) => body_type_info
                };

                match (assert_type_info, body_type_info) {
                    (TypeInfo::Interface(..), TypeInfo::Struct(..)) => {
                        eprintln!("ERROR: Can't assert an interface type on a struct type");
                        exit(1);
                    }
                    (TypeInfo::Struct(..), TypeInfo::Interface(..)) => {
                        if !self.is_subtype_of(&expression_type, assert) {
                            eprintln!("ERROR: Asserted type {:?} is not a subtype of type {:?}", assert, expression_type);
                            exit(1);
                        }
                    }
                    (TypeInfo::Interface(..), TypeInfo::Interface(..)) => {}
                    (TypeInfo::Struct(..), TypeInfo::Struct(..)) => {}
                }

                assert.clone()
            }
            Expression::Number { .. } => {
                Type::Int
            }
            Expression::BinOp { lhs, rhs, .. } => {
                let lhs_type = self.check_expression(lhs, context);
                let rhs_type = self.check_expression(rhs, context);

                if lhs_type == Type::Int && rhs_type == Type::Int {
                    Type::Int
                } else {
                    eprintln!("ERROR: Either lhs or rhs of a binary operation does not evaluate to an integer type");
                    exit(1);
                }
            }
        }
    }

    fn is_subtype_of(&self, parent_type: &Type, child_type: &Type) -> bool {
        // a type is a subtype of itself
        if parent_type == child_type {
            return true;
        }

        // type info for child type
        let child_type_info = match self.types.get(self.type_name(child_type)) {
            None => {
                eprintln!("Type {:?} is undeclared", child_type);
                exit(1);
            }
            Some(type_info) => type_info
        };

        // methods of parent type
        let methods = match parent_type {
            Type::Int => {
                eprintln!("Integer doesn't have methods");
                exit(1);
            }
            Type::Struct(type_name) => {
                match self.types.get(type_name) {
                    None => {
                        eprintln!("Cant find type {:?}", type_name);
                        exit(1);
                    }
                    Some(type_info) => {
                        match type_info {
                            TypeInfo::Struct(..) => {
                                eprintln!("Only an interface can be a parent type");
                                exit(1);
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
            if child_type_info.method_spec(method.name).is_none() {
                eprintln!("Method {:?} of parent type {:?} is not implemented for child type {:?}", method.name, parent_type, child_type);
                return false;
            }
        }

        true
    }

    fn type_name<'a>(&self, type_: &'a Type) -> &'a str {
        match type_ {
            Type::Int => {
                eprintln!("ERROR: No type name for integer");
                exit(1);
            }
            Type::Struct(name) => name
        }
    }
}
