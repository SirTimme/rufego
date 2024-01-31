pub(crate) fn report_duplicate_type(name: &str) -> String {
    format!("Type '{name}' was already declared")
}

pub(crate) fn report_interface_implementation(method_name: &str, type_: &str) -> String {
    format!("Interface method '{method_name}' can only be implemented by struct types, provided type '{type_}' is an interface")
}

pub(crate) fn report_duplicate_method_implementation(method_name: &str, type_: &str) -> String {
    format!("Method '{method_name}' was already implemented for type '{type_}'")
}

pub(crate) fn report_unknown_receiver_type(type_: &str, method: &str) -> String {
    format!("Encountered unknown receiver type '{type_}' while declaring method '{method}'")
}

pub(crate) fn report_unknown_type_formal(type_formal: &str) -> String {
    format!("Encountered unknown type formal '{type_formal}'")
}

pub(crate) fn report_wrong_type_bound() -> String {
    String::from("Only interface types can define a type bound. Provided type was of type 'int' or 'struct'")
}

pub(crate) fn report_duplicate_field_name(field_name: &str, struct_: &str) -> String {
    format!("Encountered duplicate field name '{field_name}' in struct '{struct_}'")
}

pub(crate) fn report_duplicate_method_name(method_name: &str, interface: &str) -> String {
    format!("Encountered duplicate method name'{method_name}' in interface '{interface}'")
}

pub(crate) fn report_duplicate_type_formal(type_formal: &str, method: &str) -> String {
    format!("Encountered duplicate type formal '{type_formal}' in method '{method}'")
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

pub(crate) fn report_wrong_method_parameters(method: &str, expected: usize, actual: usize) -> String {
    format!("Method '{method}' has '{expected}' parameters but '{actual}' parameters were provided")
}

pub(crate) fn report_method_not_implemented(method: &str, type_: &str) -> String {
    format!("Method '{method}' is not implemented for type '{type_}'")
}

pub(crate) fn report_unknown_interface_method(interface: &str, method: &str) -> String {
    format!("Interface '{interface}' does not have a method named '{method}'")
}



