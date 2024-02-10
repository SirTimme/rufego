use parser::Program;
use type_checker::TypeInfos;

pub(crate) fn monomorph(program: &Program, type_infos: &TypeInfos) -> Result<(), ()> {
    println!("{:#?}", program);
    Ok(())
}