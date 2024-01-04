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
    fn check_method_specification(&self, method_spec: &MethodBody) {
        // parameters distinct?
        for (i, parameter) in method_spec.params.iter().enumerate() {
            // parameter type declared?
            self.check_type(parameter.type_);

            if method_spec.params.iter().skip(i + 1).any(|element| element.variable == parameter.variable) {
                eprintln!("ERROR: Found duplicate method parameter {:?} for method {:?}", parameter.variable, method_spec.name);
                exit(1);
            }
        }

        // return types declared?
        self.check_type(method_spec.return_type);
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
            TypeLiteral::Struct { name, fields} => {
                // field names distinct?
                for (i, field) in fields.iter().enumerate() {
                    // field type declared?
                    self.check_type(field.type_);

                    if fields.iter().skip(i + 1).any(|element| element.variable == field.variable) {
                        eprintln!("ERROR: Found duplicate struct field {:?} for struct {:?}", field.variable, name);
                        exit(1);
                    }
                }
            }
            TypeLiteral::Interface { name, methods: method_specs } => {
                // method name unique?
                for (i, method_spec) in method_specs.iter().enumerate() {
                    // method specification well formed?
                    self.check_method_specification(method_spec);

                    if method_specs.iter().skip(i + 1).any(|element| element.name == method_spec.name) {
                        eprintln!("ERROR: Found duplicate interface method {:?} for interface {:?}", method_spec.name, name);
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
                for (i, parameter) in parameters.iter().enumerate() {
                    // parameter type declared?
                    self.check_type(parameter.type_);

                    if receiver.variable == parameter.variable || parameters.iter().skip(i + 1).any(|element| element.variable == parameter.variable) {
                        eprintln!("ERROR: Found duplicate method parameter {:?}", parameter.variable);
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