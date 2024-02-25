pub(crate) fn report_invalid_subtype_base(child_type: &str, parent_type: &str) -> String {
    format!("Subtype check of child type '{child_type}' with parent type '{parent_type}' failed:")
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