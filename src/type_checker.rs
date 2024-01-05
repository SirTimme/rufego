use std::collections::{HashMap};
use std::process::exit;
use parser::{Declaration, Expression, MethodSpecification, Program, TypeLiteral};

pub(crate) struct TypeChecker<'a> {
    pub(crate) program: &'a Program<'a>,
    pub(crate) types: HashMap<&'a str, &'a Declaration<'a>>,
}

impl TypeChecker<'_> {
    pub(crate) fn check_program(&mut self) {
        self.collect_types();

        for declaration in &self.program.declarations {
            self.check_declaration(declaration);
        }
    }

    // TODO Slices statt Vecs? peg support?
    // TODO Overall structure ok? How to keep track of types?
    // TODO How to properly build typing contexts
    // TODO Typechecking of Methods
    // TODO How to global context?
    // TODO How to handle numbers?

    fn collect_types(&mut self) {
        for declaration in &self.program.declarations {
            if let Declaration::Type { literal } = declaration {
                match literal {
                    TypeLiteral::Struct { name, .. } => {
                        if self.types.contains_key(name) {
                            eprintln!("ERROR: Type {:?} already declared", name);
                            exit(1);
                        } else {
                            self.types.insert(name, declaration);
                        }
                    }
                    TypeLiteral::Interface { name, .. } => {
                        if self.types.contains_key(name) {
                            eprintln!("ERROR: Type {:?} already declared", name);
                            exit(1);
                        } else {
                            self.types.insert(name, declaration);
                        }
                    }
                }
            }
        }
    }

    /*
        Judgement t ok => type t is declared
    */
    fn check_type(&self, name: &str) {
        // is the type declared?
        if !self.types.contains_key(name) {
            eprintln!("ERROR: Use of undeclared type {:?}", name);
            exit(1);
        }
    }

    /*
        Judgment S ok => method specification S is well formed
            - all formal parameters x are distinct
            - all the types t are declared
     */
    fn check_method_specification(&self, method_specification: &MethodSpecification) {
        // parameters distinct?
        for (index, parameter) in method_specification.parameters.iter().enumerate() {
            // parameter type declared?
            self.check_type(parameter.type_);

            if method_specification.parameters.iter().skip(index + 1).any(|element| element.name == parameter.name) {
                eprintln!("ERROR: Found duplicate method parameter {:?} for method {:?}", parameter.name, method_specification.name);
                exit(1);
            }
        }

        // return types declared?
        self.check_type(method_specification.return_type);
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
                // field names distinct?
                for (index, field) in fields.iter().enumerate() {
                    // field type declared?
                    self.check_type(field.type_);

                    if fields.iter().skip(index + 1).any(|element| element.name == field.name) {
                        eprintln!("ERROR: Found duplicate struct field {:?} for struct {:?}", field.name, name);
                        exit(1);
                    }
                }
            }
            TypeLiteral::Interface { name, methods: method_specifications } => {
                // method name unique?
                for (index, method_specification) in method_specifications.iter().enumerate() {
                    // method specification well formed?
                    self.check_method_specification(method_specification);

                    if method_specifications.iter().skip(index + 1).any(|element| element.name == method_specification.name) {
                        eprintln!("ERROR: Found duplicate interface method {:?} for interface {:?}", method_specification.name, name);
                        exit(1);
                    }
                }
            }
        }
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
            Declaration::Method { receiver, parameters, return_type, body, name } => {
                // receiver type declared?
                self.check_type(receiver.type_);

                // parameters distinct?
                for (index, parameter) in parameters.iter().enumerate() {
                    // parameter type declared?
                    self.check_type(parameter.type_);

                    if receiver.name == parameter.name || parameters.iter().skip(index + 1).any(|element| element.name == parameter.name) {
                        eprintln!("ERROR: Found duplicate method parameter {:?}", parameter.name);
                        exit(1);
                    }
                }

                // return-type declared?
                self.check_type(return_type);

                // build type context
                let mut context = HashMap::new();
                context.insert(receiver.name, receiver.type_);

                for parameter in parameters {
                    context.insert(parameter.name, parameter.type_);
                }

                // determine type of body
                let expression_type = self.check_expression(body, &context);

                println!("Expression type of method {:?} is {:?}", name, expression_type)

                // TODO does expression type implement return type?
            }
        }
    }

    fn check_expression<'a>(&'a self, expression: &Expression<'a>, context: &HashMap<&str, &'a str>) -> &str {
        match expression {
            Expression::Variable { name } => {
                if let Some(var_type) = context.get(name) {
                    var_type
                } else {
                    eprintln!("ERROR: Use of unknown variable {:?}", name);
                    exit(1);
                }
            }
            Expression::MethodCall { expression, method: method_name, parameter_expressions } => {
                if let Some(Declaration::Method { receiver, name, parameters, return_type, .. }) = self.types.get(method_name) {
                    let expression_type = self.check_expression(expression, context);

                    // is the method called on the correct type?
                    if expression_type != receiver.type_ {
                        eprintln!("ERROR: Method can only be called from type {:?}", receiver.type_);
                        exit(1);
                    }

                    if parameter_expressions.len() != parameters.len() {
                        eprintln!("ERROR: Call of method {:?} does not match method signature", name);
                        exit(1);
                    }

                    for (index, expression) in parameter_expressions.iter().enumerate() {
                        let expression_type = self.check_expression(expression, context);

                        if let Some(parameter) = parameters.get(index) {
                            if expression_type == parameter.type_ {
                                continue;
                            } else {
                                eprintln!("ERROR: Method parameter {:?} has type {:?}, got type {:?} instead", parameter.name, parameter.type_, expression_type);
                                exit(1);
                            }
                        }
                    }

                    // is the return type declared?
                    self.check_type(return_type);

                    return_type
                } else {
                    eprintln!("ERROR: Expected method declaration, got type declaration instead");
                    exit(1);
                }
            }
            Expression::StructLiteral { name, field_expressions } => {
                // struct type declared?
                self.check_type(name);

                if let Some(Declaration::Type { literal }) = self.types.get(name) {
                    if let TypeLiteral::Struct { name, fields } = literal {
                        // expressions the same length as struct fields?
                        if field_expressions.len() != fields.len() {
                            eprintln!("ERROR: Field expressions does not match shape of struct {:?}", name);
                            exit(1);
                        }

                        // evaluate body expressions of struct
                        for (index, expression) in field_expressions.iter().enumerate() {
                            let expression_type = self.check_expression(expression, context);

                            if let Some(field) = fields.get(index) {
                                if expression_type == field.type_ {
                                    // TODO does the expression type implements the field type (not only be the same)
                                    continue;
                                } else {
                                    eprintln!("ERROR: Field {:?} of type {:?} has type {:?}, got type {:?} instead", field.name, name, field.type_, expression_type);
                                    exit(1);
                                }
                            } else {
                                eprintln!("ERROR: Could not find field");
                                exit(1);
                            }
                        }

                        name
                    } else {
                        eprintln!("ERROR: Expected struct type, got interface type instead");
                        exit(1);
                    }
                } else {
                    eprintln!("ERROR: Expected type declaration, got method declaration instead");
                    exit(1);
                }
            }
            Expression::Select { expression, field: field_var } => {
                let type_ = self.check_expression(expression, context);

                if let Some(declaration) = self.types.get(type_) {
                    if let Declaration::Type { literal } = declaration {
                        if let TypeLiteral::Struct { name, fields } = literal {
                            if let Some(field_type) = fields.iter().find(|field| &field.name == field_var) {
                                field_type.type_
                            } else {
                                eprintln!("ERROR: Struct type {:?} doesnt have a field named {:?}", name, field_var);
                                exit(1);
                            }
                        } else {
                            eprintln!("ERROR: Expected struct type, got interface type instead");
                            exit(1);
                        }
                    } else {
                        eprintln!("ERROR: Expected type declaration, got method declaration type instead");
                        exit(1);
                    }
                } else {
                    eprintln!("ERROR: Use of unknown type {:?}", type_);
                    exit(1);
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

                assert
            }
            Expression::Number { .. } => {
                "number"
            }
            Expression::BinOp { lhs, rhs, .. } => {
                let lhs_type = self.check_expression(lhs, context);
                let rhs_type = self.check_expression(rhs, context);

                if lhs_type == "number" && rhs_type == "number" {
                    "number"
                } else {
                    eprintln!("ERROR: Addition and multiplication can only be applied to numbers");
                    exit(1);
                }
            }
        }
    }
}