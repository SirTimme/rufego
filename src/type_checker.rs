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
    // TODO Overall structure ok? How to keep track of types?
    // TODO How to properly build typing contexts
    // TODO Typechecking of Methods

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

            if method_spec.params.iter().skip(i + 1).any(|element| element.name == parameter.name) {
                eprintln!("ERROR: Found duplicate method parameter {:?} for method {:?}", parameter.name, method_spec.name);
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
            TypeLiteral::Struct { name, fields } => {
                // field names distinct?
                for (i, field) in fields.iter().enumerate() {
                    // field type declared?
                    self.check_type(field.type_);

                    if fields.iter().skip(i + 1).any(|element| element.name == field.name) {
                        eprintln!("ERROR: Found duplicate struct field {:?} for struct {:?}", field.name, name);
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
                // receiver type declared?
                self.check_type(receiver.type_);

                // parameters distinct?
                for (i, parameter) in parameters.iter().enumerate() {
                    // parameter type declared?
                    self.check_type(parameter.type_);

                    if receiver.name == parameter.name || parameters.iter().skip(i + 1).any(|element| element.name == parameter.name) {
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

                println!("Expression Type is {:?}", expression_type);

                // TODO does expression type implement return type?
            }
        }
    }

    fn check_expression<'a>(&'a self, expression: &Expression, context: &HashMap<&str, &'a str>) -> &str {
        // TODO loop necessary?

        match expression {
            Expression::Variable { name } => {
                if let Some(type_) = context.get(name) {
                    type_
                } else {
                    eprintln!("ERROR: Use of unknown variable {:?}", name);
                    exit(1);
                }
            }
            Expression::MethodCall { .. } => {
                "TODO"
            }
            Expression::StructLiteral { name, field_expressions } => {
                // struct type declared?
                self.check_type(name);

                if let TypeLiteral::Struct { name, fields } = self.get_type_literal(name) {
                    // expressions the same length as struct fields?
                    if field_expressions.len() != fields.len() {
                        eprintln!("ERROR: Field expressions does not match shape of struct {:?}", name);
                        exit(1);
                    }

                    // evaluate body expressions of struct
                    for (i, expression) in field_expressions.iter().enumerate() {
                        let expression_type = self.check_expression(expression, context);
                        if let Some(field) = fields.get(i) {
                            if expression_type == field.type_ {
                                // TODO does the expression type implements the field type (not only be the same)
                                continue
                            } else {
                                eprintln!("ERROR: Field {:?} of type {:?} has type {:?}, got type {:?} instead", field.name, name, field.type_, expression_type);
                                exit(1);
                            }
                        } else {
                            eprintln!("ERROR: Could not find a field");
                            exit(1);
                        }
                    }

                    name
                } else {
                    eprintln!("ERROR: Expected struct type, got interface type instead");
                    exit(1);
                }
            }
            Expression::Select { expression, field: field_var } => {
                let type_ = self.check_expression(expression, context);

                if let Some(struct_) = self.types.get(type_) {
                    if let Declaration::Type { literal } = struct_ {
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
            Expression::TypeAssertion { .. } => {
                "TODO"
            }
        }
    }

    fn get_type_literal(&self, type_: &str) -> &TypeLiteral {
        if let Some(struct_) = self.types.get(type_) {
            if let Declaration::Type { literal } = struct_ {
                literal
            } else {
                eprintln!("ERROR: Expected type declaration, got method declaration type instead");
                exit(1);
            }
        } else {
            eprintln!("ERROR: Use of unknown type {:?}", type_);
            exit(1);
        }
    }
}