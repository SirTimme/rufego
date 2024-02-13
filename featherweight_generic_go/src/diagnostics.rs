pub(crate) fn report_duplicate_type_declaration(type_name: &str) -> String {
    format!("Declaration of type '{type_name}' failed. Type was already declared")
}

pub(crate) enum MethodImplementationError {
    InterfaceReceiverType,
    UnknownReceiverType,
    DuplicateMethodImplementation,
}

pub(crate) fn report_invalid_method_receiver(method_name: &str, type_: &str, error_cause: MethodImplementationError) -> String {
    let cause = match error_cause {
        MethodImplementationError::InterfaceReceiverType => format!("Provided receiver type '{type_}' is an 'interface' type"),
        MethodImplementationError::UnknownReceiverType => format!("Tried to implement method for undeclared receiver type {type_}"),
        MethodImplementationError::DuplicateMethodImplementation => format!("Method is already implemented for receiver type '{type_}'"),
    };

    format!("Implementation of method '{method_name}' failed: {cause}")
}

pub(crate) enum TypeDeclarationError {
    DuplicateFieldStruct,
    DuplicateMethodInterface,
}

pub(crate) fn report_duplicate_literal_parameter(field_name: &str, literal_name: &str, error_cause: TypeDeclarationError) -> String {
    let cause = match error_cause {
        TypeDeclarationError::DuplicateFieldStruct => format!("Got duplicate field declaration with name '{field_name}' in struct type '{literal_name}'"),
        TypeDeclarationError::DuplicateMethodInterface => format!("Got duplicate method specification with name '{field_name}' in interface type '{literal_name}'"),
    };

    format!("Declaration of type literal '{literal_name}' failed: {cause}")
}

pub(crate) fn report_duplicate_method_spec_parameter(method_name: &str, interface_name: &str, parameter_name: &str) -> String {
    format!("Declaration of method specification '{method_name}' for interface '{interface_name}' failed: Got duplicate parameter with name'{parameter_name}'")
}

pub(crate) fn report_duplicate_method_parameter(receiver_type: &str, method_name: &str, parameter_name: &str) -> String {
    format!("Declaration of method '{method_name}' for receiver type '{receiver_type}' failed: Got duplicate parameter with name '{parameter_name}'")
}

pub(crate) enum CheckEnvironment {
    MethodSpec,
    Method,
    Literal,
}

pub(crate) fn report_unknown_type(environment_name: &str, type_name: &str, check_environment: &CheckEnvironment) -> String {
    let cause = match check_environment {
        CheckEnvironment::Literal => format!("It is unknown in type literal '{environment_name}'"),
        CheckEnvironment::MethodSpec => format!("It is unknown in method specification '{environment_name}'"),
        CheckEnvironment::Method => format!("It is unknown in method declaration '{environment_name}'"),
    };

    format!("Checking of type '{type_name}' failed: {cause}")
}

pub(crate) fn report_unknown_type_parameter(environment_name: &str, type_parameter: &str, check_environment: &CheckEnvironment) -> String {
    let cause = match check_environment {
        CheckEnvironment::Literal => format!("It is unknown in type literal '{environment_name}'"),
        CheckEnvironment::MethodSpec => format!("It is unknown in method specification '{environment_name}'"),
        CheckEnvironment::Method => format!("It is unknown in method declaration '{environment_name}'"),
    };

    format!("Checking of type parameter '{type_parameter}' failed: {cause}")
}

pub(crate) fn report_invalid_variable(method_name: &str, variable_name: &str) -> String {
    format!("Checking of body expression of method {method_name} failed: Variable '{variable_name}' is unknown in this context")
}

pub(crate) fn report_invalid_method_call_arg_count_mismatch(method_name: &str, expression_type: &str, method_call: &str, expected: usize, actual: usize) -> String {
    format!("Checking of body expression of method '{method_name}' failed: Called method '{method_call}' on receiver type '{expression_type}' expected '{expected}' parameters but '{actual}' parameters were provided")
}

pub(crate) fn report_invalid_method_call_number(method_name: &str) -> String {
    format!("Checking of body expression of method '{method_name}' failed: Expression evaluates to an integer type on which no methods can be called")
}

pub(crate) fn report_invalid_method_call_not_implemented(method_name: &str, called_method: &str, receiver_type: &str) -> String {
    format!("Checking of body expression of method '{method_name}' failed: Called method '{called_method}' is not implemented for receiver type '{receiver_type}'")
}

pub(crate) enum StructLiteralError {
    InterfaceType,
    UnknownType,
}

pub(crate) fn report_invalid_struct_literal(method_name: &str, type_name: &str, error_cause: StructLiteralError) -> String {
    let cause = match error_cause {
        StructLiteralError::InterfaceType => format!("Tried to instantiate an interface type '{type_name}'"),
        StructLiteralError::UnknownType => format!("Tried to instantiate an undeclared type '{type_name}'"),
    };

    format!("Checking of body expression of method '{method_name}' failed: {cause}")
}

pub(crate) fn report_invalid_select_unknown_field(method_name: &str, type_name: &str, field_name: &str) -> String {
    format!("Checking of body expression of method '{method_name}' failed: Struct type '{type_name}' does not have a field named '{field_name}'")
}

pub(crate) fn report_invalid_select_interface(method_name: &str, type_name: &str) -> String {
    format!("Checking of body expression of method '{method_name}' failed: Expression evaluates to an interface type '{type_name}' on which no select can be made")
}

pub(crate) fn report_invalid_assert_type_interface(method_name: &str, asserted_type: &str, assertion_type: &str) -> String {
    format!("Checking of body expression of method '{method_name}' failed: Tried to assert interface type '{assertion_type}' on expression type '{asserted_type}'")
}

pub(crate) enum BinOpError {
    Lhs,
    Rhs,
    Both,
}

pub(crate) fn report_invalid_bin_op(method_name: &str, lhs_type: &str, rhs_type: &str, error_cause: BinOpError) -> String {
    let cause = match error_cause {
        BinOpError::Lhs => format!("Left side of binary operation evaluates to type '{lhs_type}' but expected left side to be of type integer"),
        BinOpError::Rhs => format!("Right side of binary operation evaluates to type '{rhs_type}' but expected right side to be of type integer"),
        BinOpError::Both => format!("Left side of binary operation evaluates to type '{lhs_type}', right side evaluates to type '{rhs_type}' but expected both sides to be of type integer"),
    };

    format!("Checking of body expression of method '{method_name}' failed: '{cause}'")
}

pub(crate) fn report_invalid_subtype_method(expression_type: &str, return_type: &str, method_name: &str, receiver_type: &str) -> String {
    format!("Subtype check for expression type '{expression_type}' to be subtype of declared return type '{return_type}' of method '{method_name}' for receiver type '{receiver_type}' failed:")
}

pub(crate) fn report_invalid_subtype_method_call(method_name: &str, parameter_name: &str, expected_parameter_type: &str, actual_parameter_name: &str) -> String {
    format!("Method '{method_name}' declares parameter '{parameter_name}' to be of type '{expected_parameter_type}' but type '{actual_parameter_name}' is provided which is not a subtype")
}

pub(crate) fn report_method_not_implemented(method_name: &str, child_type: &str, parent_type: &str) -> String {
    format!("Method '{method_name}' of parent type '{parent_type}' is not implemented for child type '{child_type}'")
}

pub(crate) enum SubTypeNumberError {
    Lhs,
    Rhs,
}

pub(crate) fn report_invalid_subtype_number(child_type: &str, parent_type: &str, error_cause: SubTypeNumberError) -> String {
    match error_cause {
        SubTypeNumberError::Lhs => format!("Child type is of type '{child_type}' which can only be subtype of another integer type"),
        SubTypeNumberError::Rhs => format!("Parent type is of type '{parent_type}' which can only be parent type of another integer type"),
    }
}

pub(crate) fn report_invalid_subtype_base(child_type: &str, parent_type: &str) -> String {
    format!("Subtype check of child type '{child_type}' with parent type '{parent_type}' failed:")
}

pub(crate) fn report_invalid_subtype_struct_literal(child_type: &str, parent_type: &str) -> String {
    format!("Tried to compare struct type '{parent_type}' with struct type '{child_type}' which cannot be a subtype")
}

pub(crate) fn report_invalid_subtype_return_type_mismatch(method_name: &str, child_type: &str, child_type_return_type: &str, parent_type: &str, parent_type_return_type: &str) -> String {
    format!("Method '{method_name}' of parent type '{parent_type}' has return type '{parent_type_return_type}' but the implementation for child type '{child_type}' has return type '{child_type_return_type}'")
}

pub(crate) fn report_invalid_subtype_parameter_arg_mismatch(method_name: &str, child_type: &str, child_parameter_count: usize, parent_type: &str, parent_parameter_count: usize) -> String {
    format!("Method '{method_name}' of parent type '{parent_type}' has '{parent_parameter_count}' parameters but implementation of child type '{child_type}' has '{child_parameter_count}' parameters")
}

pub(crate) fn report_invalid_subtype_parameter_type_mismatch(parameter_name: &str, method_name: &str, parent_type: &str, parent_parameter_type: &str, child_type: &str, child_parameter_type: &str) -> String {
    format!("Method parameter '{parameter_name}' of method '{method_name}' of parent type '{parent_type}' has type '{parent_parameter_type}' parameters but implementation of child type '{child_type}' has type '{child_parameter_type}'")
}

pub(crate) fn report_invalid_type_bound_arg_mismatch(type_name: &str, expected: usize, actual: usize) -> String {
    format!("Checking type bound of type {type_name} failed: The declared type has '{expected}' type parameters but '{actual}' type parameters were provided")
}

pub(crate) fn report_duplicate_type_formal(surrounding_type: &str, type_parameter: &str) -> String {
    format!("Checking the concatenated type environment of type '{surrounding_type}' failed: Encountered duplicate type parameter '{type_parameter}'")
}

pub(crate) fn report_invalid_type_parameter(surrounding_type: &str, type_parameter: &str, actual_type: &str) -> String {
    format!("Checking type parameter '{type_parameter}' inside type '{surrounding_type}' failed: Type parameter is of type '{actual_type}' which is a struct type")
}