use std::collections::{HashMap};
use std::process::exit;
use parser::{Declaration, Expression, MethodBody, Program, TypeLiteral};

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
    // TODO Method overloading? typing rules allows that?
    // TODO Overall structure ok? How to keep track of types?
    // TODO

    fn collect_types(&mut self) {
        for declaration in &self.program.declarations {
            if let Declaration::Type { literal } = declaration {
                match literal {
                    TypeLiteral::Struct { name, .. } => {
                        if self.types.contains_key(name) {
                            eprintln!("ERROR Duplicate struct type {name}");
                            exit(1);
                        } else {
                            self.types.insert(name, declaration);
                        }
                    }
                    TypeLiteral::Interface { name, .. } => {
                        if self.types.contains_key(name) {
                            eprintln!("ERROR Duplicate interface type {name}");
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
    fn check_type(&self, type_name: &str) {
        // is the type declared?
        if !self.types.contains_key(type_name) {
            eprintln!("ERROR Use of undeclared type: {type_name}");
            exit(1);
        }
    }

    /*
        Judgment S ok => method specification S is well formed
            - all formal parameters x are distinct
            - all the types t are declared
     */
    fn check_method_specification(&self, method_body: &MethodBody) {
        // parameters distinct?
        for (i, item) in method_body.params.iter().enumerate() {
            // parameter type declared?
            self.check_type(item.type_);

            if method_body.params.iter().skip(i + 1).any(|x| x.variable == item.variable) {
                eprintln!("ERROR Duplicate method parameter {:?}", item.variable);
                exit(1);
            }
        }

        // return types declared?
        self.check_type(method_body.return_type);
    }

    /*
        Judgement T ok => type literal T is well formed
            Structure:
                - all field names are distinct
                - all types declared
            Interface:
                - all its method specifications are well formed
     */
    fn check_type_literal(&self, type_literal: &TypeLiteral) {
        match type_literal {
            TypeLiteral::Struct { fields, .. } => {
                // field names distinct?
                for (i, field) in fields.iter().enumerate() {
                    // field type declared?
                    self.check_type(field.type_);

                    if fields.iter().skip(i + 1).any(|x| x.variable == field.variable) {
                        eprintln!("ERROR Duplicate struct field {:?}", field.variable);
                        exit(1);
                    }
                }
            }
            TypeLiteral::Interface { methods, .. } => {
                // method specifications distinct?
                for (i, method_spec) in methods.iter().enumerate() {
                    // method specification well formed?
                    self.check_method_specification(method_spec);

                    if methods.iter().skip(i + 1).any(|x| x == method_spec) {
                        eprintln!("ERROR Duplicate method specification {:?}", method_spec);
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
            Declaration::Method { receiver, parameters, return_type, body, .. } => {
                // TODO

                // receiver type declared?
                self.check_type(receiver.type_);

                // parameters distinct?
                for (i, item) in parameters.iter().enumerate() {
                    // parameter type declared?
                    self.check_type(item.type_);

                    if receiver.variable == item.variable || parameters.iter().skip(i + 1).any(|x| x.variable == item.variable) {
                        eprintln!("ERROR Duplicate method parameter {:?}", item.variable);
                        exit(1);
                    }
                }

                // return-type declared?
                self.check_type(return_type);

                // expression type implements return type=
                match body {
                    Expression::Variable { .. } => {}
                    Expression::MethodCall { .. } => {}
                    Expression::StructLiteral { .. } => {}
                    Expression::Select { .. } => {}
                    Expression::TypeAssertion { .. } => {}
                    Expression::Number { .. } => {}
                    Expression::BinOp { .. } => {}
                }
            }
        }
    }
}