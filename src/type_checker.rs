use std::collections::{HashMap};
use std::process::exit;
use parser::{Binding, Declaration, Expression, MethodDeclaration, MethodSpecification, Program, TypeLiteral};

pub(crate) struct TypeChecker<'a> {
    pub(crate) program: &'a Program<'a>,
    pub(crate) types: HashMap<&'a str, TypeInfo<'a>>,
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub(crate) enum Type<'a> {
    Int,
    Struct(&'a str),
}

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
                        eprintln!("ERROR: Method declaration for unknown type");
                        exit(1);
                    }
                    Some(TypeInfo::Interface(..)) => {
                        eprintln!("ERROR: Cant implement interface method for interface");
                        exit(1);
                    }
                    Some(TypeInfo::Struct(.., methods)) => {
                        if let Some(_) = methods.insert(method.specification.name, method) {
                            eprintln!("ERROR: Duplicate method declaration with name {:?} for type {:?}", method.specification.name, method.receiver.type_);
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
        println!("Return type of main is {:?}", self.check_expression(&self.program.expression, &HashMap::new()));
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
                        eprintln!("ERROR: Found duplicate method parameter {:?} in method {:?}", parameter.name, specification.name);
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
                if expression_type != specification.return_type {
                    eprintln!("ERROR: Method {:?} has return type {:?}, body evaluates to type {:?} instead", specification.name, specification.return_type, expression_type);
                    exit(1);
                }

                // TODO check also for subtype
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
                        eprintln!("ERROR: Found duplicate struct field {:?} for struct {:?}", field.name, name);
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
                        eprintln!("ERROR: Found duplicate interface method {:?} for interface {:?}", method_specification.name, name);
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
                eprintln!("ERROR: Found duplicate method parameter {:?} for method {:?}", parameter.name, method_specification.name);
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
                    eprintln!("ERROR: Use of undeclared type {:?}", name);
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
                    eprintln!("ERROR: Use of unknown variable {:?}", name);
                    exit(1);
                }
            }
            Expression::MethodCall { expression, method, parameter_expressions } => {
                let type_name = match self.check_expression(expression, context) {
                    Type::Int => {
                        eprintln!("ERROR: Method cant be called from an integer");
                        exit(1);
                    }
                    Type::Struct(name) => name
                };

                match self.types.get(type_name) {
                    None => {
                        eprintln!("ERROR: Expression evaluates to an unknown type");
                        exit(1);
                    }
                    Some(type_info) => {
                        match type_info {
                            TypeInfo::Struct(.., methods) => {
                                match methods.get(method) {
                                    None => {
                                        eprintln!("ERROR: Use of unknown method {:?} for type {:?}", method, type_name);
                                        exit(1);
                                    }
                                    Some(declaration) => {
                                        if parameter_expressions.len() != declaration.specification.parameters.len() {
                                            eprintln!("ERROR: Call of method {:?} does not match method signature", method);
                                            exit(1);
                                        }

                                        for (index, expression) in parameter_expressions.iter().enumerate() {
                                            if let Some(parameter) = declaration.specification.parameters.get(index) {
                                                // evaluate type of supplied parameter
                                                let expression_type = self.check_expression(expression, context);

                                                if expression_type != parameter.type_ {
                                                    eprintln!("ERROR: Method parameter {:?} has type {:?}, got type {:?} instead", parameter.name, parameter.type_, expression_type);
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
                        eprintln!("ERROR: Use of an undeclared struct literal {:?}", name);
                        exit(1);
                    }
                    Some(type_info) => {
                        match type_info {
                            TypeInfo::Struct(fields, ..) => {
                                // correct amount of parameters supplied?
                                if field_expressions.len() != fields.len() {
                                    eprintln!("ERROR: Field expressions does not match shape of struct {:?}", name);
                                    exit(1);
                                }

                                for (index, expression) in field_expressions.iter().enumerate() {
                                    if let Some(field) = fields.get(index) {
                                        // evaluate type of supplied parameter
                                        let field_type = self.check_expression(expression, context);

                                        // TODO check also for subtype
                                        if field_type != field.type_ {
                                            eprintln!("ERROR: Field {:?} of type {:?} has type {:?}, got type {:?} instead", field.name, name, field.type_, field_type);
                                            exit(1);
                                        }
                                    } else {
                                        eprintln!("ERROR: Could not find field");
                                        exit(1);
                                    }
                                }

                                Type::Struct(name)
                            }
                            TypeInfo::Interface(_) => {
                                eprintln!("ERROR: Cant instantiate an interface type");
                                exit(1);
                            }
                        }
                    }
                }
            }
            Expression::Select { expression, field: field_var } => {
                let type_name = match self.check_expression(expression, context) {
                    Type::Int => {
                        eprintln!("ERROR: Cant select on an integer type");
                        exit(1);
                    },
                    Type::Struct(name) => name,
                };

                match self.types.get(type_name) {
                    None => {
                        eprintln!("ERROR: Use of unknown type {:?}", type_name);
                        exit(1);
                    }
                    Some(type_info) => {
                        match type_info {
                            TypeInfo::Struct(fields, _) => {
                                if let Some(field) = fields.iter().find(|field| &field.name == field_var) {
                                    // TODO w/o clone?
                                    field.type_.clone()
                                } else {
                                    eprintln!("ERROR: Struct type {:?} doesnt have a field named {:?}", type_name, field_var);
                                    exit(1);
                                }
                            }
                            TypeInfo::Interface(_) => {
                                eprintln!("ERROR: Cant select on an interface type");
                                exit(1);
                            }
                        }
                    }
                }
            }
            Expression::TypeAssertion { expression, assert } => {
                // asserted type declared?
                self.check_type(assert);

                let expression_type = self.check_expression(expression, context);

                if &expression_type != assert {
                    eprintln!("ERROR Asserted type was {:?}, actual type was {:?}", assert, expression_type);
                    exit(1);
                }

                // TODO w/o clone?
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
                    eprintln!("ERROR: Addition and multiplication can only be applied to numbers");
                    exit(1);
                }
            }
        }
    }
}
