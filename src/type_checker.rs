use std::collections::{HashMap, HashSet};
use std::process::exit;
use parser::{Binding, Declaration, Expression, MethodBody, Program, TypeLiteral};

pub(crate) struct TypeChecker<'a> {
    pub(crate) program: &'a Program<'a>,
    pub(crate) structs: HashMap<&'a str, &'a Vec<Binding<'a>>>,
    pub(crate) interfaces: HashMap<&'a str, &'a Vec<MethodBody<'a>>>,
    pub(crate) types: HashSet<&'a str>,
}

impl TypeChecker<'_> {
    pub(crate) fn check_program(&mut self) {
        self.collect_types();

        for declaration in &self.program.declarations {
            match declaration {
                Declaration::Type { literal } => {
                    match literal {
                        TypeLiteral::Struct { fields, .. } => {
                            self.check_struct(fields);
                        }
                        TypeLiteral::Interface { methods, .. } => {
                            self.check_interface(methods);
                        }
                    }
                }
                Declaration::Method { receiver, parameters, return_type, body, .. } => {
                    self.check_method(receiver, parameters, return_type, body)
                }
            }
        }
    }

    fn collect_types(&mut self) {
        for declaration in &self.program.declarations {
            if let Declaration::Type { literal } = declaration {
                match literal {
                    TypeLiteral::Struct { name, fields } => {
                        if !self.types.insert(name) {
                            eprintln!("ERROR Duplicate struct type {name}");
                            exit(1);
                        } else {
                            self.types.insert(name);
                            self.structs.insert(name, fields);
                        }
                    }
                    TypeLiteral::Interface { name, methods } => {
                        if !self.types.insert(name) {
                            eprintln!("ERROR Duplicate interface type {name}");
                            exit(1);
                        } else {
                            self.types.insert(name);
                            self.interfaces.insert(name, methods);
                        }
                    }
                }
            }
        }
    }

    /*
    T ok = type literal T is well formed
    Struct:
        - all field names distinct
        - all types declared
     */
    fn check_struct(&self, fields: &Vec<Binding>) {
        // fields distinct?
        for (i, item) in fields.iter().enumerate() {
            if fields.iter().skip(i + 1).any(|x| x == item) {
                eprintln!("ERROR Duplicate struct field {:?} with type {:?}", item.variable, item.type_);
                exit(1);
            }
        }

        // field types declared?
        for field in fields {
            self.check_binding(field);
        }
    }

    /*
    T ok = type literal T is well formed
    Interface:
        - all method specifications are well formed
     */
    fn check_interface(&self, methods: &Vec<MethodBody>) {
        // are the methods distinct?
        for (i, item) in methods.iter().enumerate() {
            if methods.iter().skip(i + 1).any(|x| x == item) {
                eprintln!("ERROR Duplicate interface method {:?}", item.name);
                exit(1);
            }
        }

        // all types declared?
        for method in methods {
            for parameter in &method.params {
                self.check_binding(parameter);
            }

            self.check_type(method.return_type);
        }
    }

    /*
    D ok = declaration D is well formed
    Method:
       - Receiver distinct
       - Formal parameters distinct
       - All typed declared
       - Method body well formed
       - expression type must implement return type
    */
    fn check_method(&self, receiver: &Binding, parameters: &Vec<Binding>, return_type: &str, body: &Expression) {
        // receiver type declared?
        self.check_binding(receiver);

        // parameters distinct?
        for (i, item) in parameters.iter().enumerate() {
            if parameters.iter().skip(i + 1).any(|x| x == item) {
                eprintln!("ERROR Duplicate method parameter {:?} with type {:?}", item.variable, item.type_);
                exit(1);
            }
        }

        // return-type at least subtype?

    }

    /*
    t ok = type t is well formed
       - type t is declared
    */
    fn check_type(&self, type_name: &str) {
        // is the type declared?
        if !self.types.contains(type_name) {
            eprintln!("ERROR Use of undeclared type: {type_name}");
            exit(1);
        }
    }

    fn check_binding(&self, binding: &Binding) {
        self.check_type(binding.type_)
    }
}

/*
- Methodoverloading? gleicher Methodenname in Interfaces
- 2 Vecs auf Duplicates überprüfen bessere Option?
 */