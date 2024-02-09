pub(crate) fn report_duplicate_declaration_error(type_name: &str) -> String {
    format!("Declaration of type '{type_name}' failed. Type was already declared")
}

pub(crate) enum MethodImplementationError {
    InterfaceReceiverType,
    UnknownReceiverType,
    DuplicateMethodImplementation,
}

pub(crate) fn report_method_implementation_error(method_name: &str, type_: &str, error_cause: MethodImplementationError) -> String {
    let cause = match error_cause {
        MethodImplementationError::InterfaceReceiverType => format!("Provided receiver type '{type_}' is an 'interface' type"),
        MethodImplementationError::UnknownReceiverType => format!("Tried to implement method for undeclared receiver type {type_}"),
        MethodImplementationError::DuplicateMethodImplementation => format!("Method is already implemented for receiver type '{type_}'"),
    };

    format!("Implementation of method '{method_name}' failed: {cause}")
}

pub(crate) enum TypeBoundError {
    StructType,
    UnknownType,
}

pub(crate) fn report_type_bound_literal_error(literal_name: &str, actual_type: &str, type_parameter: &str, error_cause: TypeBoundError) -> String {
    let cause = match error_cause {
        TypeBoundError::StructType => format!("Got struct type '{actual_type}' for type parameter '{type_parameter}' in type bound"),
        TypeBoundError::UnknownType => format!("Got unknown type '{actual_type}' for type parameter '{type_parameter}' in type bound"),
    };

    format!("Declaration of type literal '{literal_name}' failed: {cause}")
}

pub(crate) fn report_type_bound_method_spec_error(method_name: &str, interface: &str, actual_type: &str, type_parameter: &str, error_cause: TypeBoundError) -> String {
    let cause = match error_cause {
        TypeBoundError::StructType => format!("Got struct type '{actual_type}' for type parameter '{type_parameter}' in type bound"),
        TypeBoundError::UnknownType => format!("Got unknown type '{actual_type}' for type parameter '{type_parameter}' in type bound"),
    };

    format!("Declaration of method specification '{method_name}' for interface '{interface}' failed: {cause}")
}

pub(crate) fn report_type_bound_method_receiver_error(method_name: &str, receiver_type: &str, actual_type: &str, type_parameter: &str, error_cause: TypeBoundError) -> String {
    let cause = match error_cause {
        TypeBoundError::StructType => format!("Got struct type '{actual_type}' for type parameter '{type_parameter}' in type bound of the receiver"),
        TypeBoundError::UnknownType => format!("Got unknown type '{actual_type}' for type parameter '{type_parameter}' in type bound of the receiver"),
    };

    format!("Declaration of method '{method_name}' for receiver type '{receiver_type}' failed: {cause}")
}

pub(crate) fn report_type_bound_method_error(method_name: &str, receiver_type: &str, actual_type: &str, type_parameter: &str, error_cause: TypeBoundError) -> String {
    let cause = match error_cause {
        TypeBoundError::StructType => format!("Got struct type '{actual_type}' for type parameter '{type_parameter}' in type bound of the method"),
        TypeBoundError::UnknownType => format!("Got unknown type '{actual_type}' for type parameter '{type_parameter}' in type bound of the method"),
    };

    format!("Declaration of method '{method_name}' for receiver type '{receiver_type}' failed: {cause}")
}

pub(crate) fn report_duplicate_field_name(field_name: &str, struct_: &str) -> String {
    format!("Encountered duplicate field name '{field_name}' in struct '{struct_}'")
}

pub(crate) fn report_duplicate_method_name(method_name: &str, interface: &str) -> String {
    format!("Encountered duplicate method name'{method_name}' in interface '{interface}'")
}

pub(crate) fn report_duplicate_method_parameter(parameter: &str, method: &str) -> String {
    format!("Encountered duplicate method parameter '{parameter}' in method '{method}'")
}

pub(crate) fn report_unknown_type_parameter(parameter: &str) -> String {
    format!("Encountered unknown type parameter '{parameter}'")
}

pub(crate) fn report_unknown_type(type_: &str) -> String {
    format!("Encountered undeclared type '{type_}'")
}

pub(crate) fn report_unknown_variable(variable: &str) -> String {
    format!("Encountered unknown variable '{variable}'")
}

pub(crate) fn report_invalid_binop() -> String {
    String::from("One or both sides of a binary operation does not evaluate to a number type")
}

pub(crate) fn report_assert_type_mismatch(body: &str, assert: &str) -> String {
    format!("Expression of assertion evaluated to a type of '{body}', asserted type was '{assert}'")
}

pub(crate) fn report_select_interface() -> String {
    String::from("Select-Expression evaluated to an interface type")
}

pub(crate) fn report_unknown_field(field: &str, struct_: &str) -> String {
    format!("Encountered unknown field '{field}' for struct type '{struct_}'")
}

pub(crate) fn report_unknown_struct_literal(struct_: &str) -> String {
    format!("Encountered unknown struct literal '{struct_}'")
}

pub(crate) fn report_struct_literal_interface(interface: &str) -> String {
    format!("Encountered interface type '{interface}' in struct literal expression which cant be instantiated")
}

pub(crate) fn report_literal_wrong_argcount(struct_: &str, expected: usize, actual: usize) -> String {
    format!("Struct type '{struct_}' has '{expected}' fields but '{actual}' values were provided")
}

pub(crate) fn report_subtype_type_mismatch(child_type: &str, parent_type: &str) -> String {
    format!("Subtype check failed. Child type was of type '{child_type}' and parent type was of type '{parent_type}'")
}

pub(crate) fn report_wrong_method_parameters(method: &str, expected: usize, actual: usize) -> String {
    format!("Method '{method}' has '{expected}' parameters but '{actual}' parameters were provided")
}

pub(crate) fn report_method_not_implemented(method: &str, type_: &str) -> String {
    format!("Method '{method}' is not implemented for type '{type_}'")
}