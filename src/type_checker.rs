use std::collections::{HashMap};
use std::process::exit;
use parser::{Binding, Declaration, Expression, MethodDeclaration, MethodSpecification, Program, TypeLiteral};

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
        // check all type declarations
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

        // check all method declarations
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
        // are the declarations well formed?
        for declaration in &self.program.declarations {
            self.check_declaration(declaration);
        }

        // is the body well formed in the empty context?
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
                // is the type literal well formed?
                self.check_type_literal(literal);
            }
            Declaration::Method(MethodDeclaration { receiver, specification, body }) => {
                // is the receiver type declared?
                self.check_type(&Type::Struct(receiver.type_));

                for (index, parameter) in specification.parameters.iter().enumerate() {
                    // is the parameter type declared?
                    self.check_type(&parameter.type_);

                    // are the parameter names distinct?
                    if receiver.name == parameter.name || specification.parameters.iter().skip(index + 1).any(|element| element.name == parameter.name) {
                        eprintln!("ERROR: Duplicate parameter name {:?} in method {:?}", parameter.name, specification.name);
                        exit(1);
                    }
                }

                // is the return-type declared?
                self.check_type(&specification.return_type);

                // build type context
                let mut context = HashMap::new();
                context.insert(receiver.name, Type::Struct(receiver.type_));

                for parameter in &specification.parameters {
                    context.insert(parameter.name, parameter.type_.clone());
                }

                // evaluate type of body expression
                let expression_type = self.check_expression(body, &context);

                // is the body type at least a subtype of the return type?
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
                    // is the field type declared?
                    self.check_type(&field.type_);

                    // are the field names distinct?
                    if fields.iter().skip(index + 1).any(|element| element.name == field.name) {
                        eprintln!("ERROR: Duplicate field name {:?} for struct {:?}", field.name, name);
                        exit(1);
                    }
                }
            }
            TypeLiteral::Interface { name, methods: method_specifications } => {
                for (index, method_specification) in method_specifications.iter().enumerate() {
                    // is the method specification well formed?
                    self.check_method_specification(method_specification);

                    // are the method names unique?
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
            // is the parameter type declared?
            self.check_type(&parameter.type_);

            // are the method parameters distinct?
            if method_specification.parameters.iter().skip(index + 1).any(|element| element.name == parameter.name) {
                eprintln!("ERROR: Duplicate parameter name {:?} for method {:?}", parameter.name, method_specification.name);
                exit(1);
            }
        }

        // is the return type declared?
        self.check_type(&method_specification.return_type);
    }

    /*
        Judgement t ok => type t is declared
    */
    fn check_type(&self, type_: &Type) {
        // is the type declared?
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
                // variable known in this context?
                if let Some(var_type) = context.get(name) {
                    var_type.clone()
                } else {
                    eprintln!("ERROR: Variable {:?} is unknown in this context", name);
                    exit(1);
                }
            }
            Expression::MethodCall { expression, method, parameter_expressions } => {
                // evaluate type of the body expression
                let expression_type = self.check_expression(expression, context);

                // typeinfo for the body expression
                let type_info = self.types.get(self.type_name(&expression_type)).expect("Expression can't evaluate to an unknown type");

                match type_info {
                    TypeInfo::Struct(.., methods) => {
                        // is the method implemented for this type?
                        match methods.get(method) {
                            None => {
                                eprintln!("ERROR: Method {:?} isn't implemented for type {:?}", method, expression_type);
                                exit(1);
                            }
                            Some(declaration) => {
                                // correct amount of parameters supplied?
                                if parameter_expressions.len() != declaration.specification.parameters.len() {
                                    eprintln!("ERROR: Method {:?} expects {:?} parameters but {:?} parameters were supplied", method, declaration.specification.parameters.len(), parameter_expressions.len());
                                    exit(1);
                                }

                                // does the types of the parameter expressions match the types of the method parameters?
                                for (index, expression) in parameter_expressions.iter().enumerate() {
                                    if let Some(parameter) = declaration.specification.parameters.get(index) {
                                        // evaluate type of the supplied parameter expression
                                        let expression_type = self.check_expression(expression, context);

                                        // is the parameter at least a subtype of the method parameter?
                                        if !self.is_subtype_of(&parameter.type_, &expression_type) {
                                            eprintln!("ERROR: Method parameter {:?} of method {:? } has type {:?} but type {:?} was supplied", parameter.name, declaration.specification.name, parameter.type_, expression_type);
                                            exit(1);
                                        }
                                    }
                                }

                                // is the return type declared?
                                self.check_type(&declaration.specification.return_type);

                                declaration.specification.return_type.clone()
                            }
                        }
                    }
                    TypeInfo::Interface(methods) => {
                        // does the method exist on the interface?
                        match methods.iter().find(|method_specification| &method_specification.name == method) {
                            None => {
                                eprintln!("ERROR: Interface {:?} doesn't have a method named {:?}", expression_type, method);
                                exit(1);
                            }
                            Some(method_specification) => {
                                // correct amount of parameters supplied?
                                if parameter_expressions.len() != method_specification.parameters.len() {
                                    eprintln!("ERROR: Method {:?} expects {:?} parameters but {:?} parameters were supplied", method, method_specification.parameters.len(), parameter_expressions.len());
                                    exit(1);
                                }

                                for (index, expression) in parameter_expressions.iter().enumerate() {
                                    if let Some(parameter) = method_specification.parameters.get(index) {
                                        // evaluate type of the supplied parameter expression
                                        let expression_type = self.check_expression(expression, context);

                                        // is the parameter at least a subtype of the method parameter?
                                        if !self.is_subtype_of(&parameter.type_, &expression_type) {
                                            eprintln!("ERROR: Method parameter {:?} of method {:? } has type {:?} but type {:?} was supplied", parameter.name, method_specification.name, parameter.type_, expression_type);
                                            exit(1);
                                        }
                                    }
                                }

                                // is the return type declared?
                                self.check_type(&method_specification.return_type);

                                method_specification.return_type.clone()
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

                let type_info = self.types.get(type_name).expect("Expression can't evaluate to an unknown type");

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
            Expression::TypeAssertion { expression, assert } => {
                self.check_type(assert);

                let assert_type_info = self.types.get(self.type_name(assert)).expect("Asserted type was checked beforehand");

                let expression_type = self.check_expression(expression, context);

                let body_type_info = self.types.get(self.type_name(&expression_type)).expect("Expression can't evaluate to an unknown type");

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

                match (lhs_type, rhs_type) {
                    (Type::Int, Type::Int) => Type::Int,
                    (Type::Struct(_), Type::Int) => {
                        eprintln!("ERROR: Left operand of a binary operation doesn't evaluate to an integer type");
                        exit(1);
                    }
                    (Type::Int, Type::Struct(_)) => {
                        eprintln!("ERROR: Right operand of a binary operation doesn't evaluate to an integer type");
                        exit(1);
                    }
                    (Type::Struct(_), Type::Struct(_)) => {
                        eprintln!("ERROR: Both operands of a binary operation don't evaluate to an integer type");
                        exit(1);
                    }
                }
            }
        }
    }

    fn is_subtype_of(&self, parent_type: &Type, child_type: &Type) -> bool {
        // a type is a subtype of itself
        if parent_type == child_type {
            return true;
        }

        let child_type_info = self.types.get(self.type_name(child_type)).expect("Function is only called with declared types");

        let methods = match parent_type {
            Type::Int => {
                eprintln!("ERROR: Integer can't be the parent type of any type");
                exit(1);
            }
            Type::Struct(type_name) => {
                match self.types.get(type_name) {
                    None => {
                        eprintln!("Type {:?} is undeclared", type_name);
                        exit(1);
                    }
                    Some(type_info) => {
                        match type_info {
                            TypeInfo::Struct(..) => {
                                eprintln!("ERROR: Only an interface can be a parent type");
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
                eprintln!("ERROR: Method {:?} of parent type {:?} is not implemented for child type {:?}", method.name, parent_type, child_type);
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
