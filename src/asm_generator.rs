use crate::types::c::ast as c_ast;
use crate::types::compile_error::CompileError;
use crate::types::x64::ast as x64_ast;

mod internal;

pub fn generate(c_program: c_ast::Program) -> Result<x64_ast::Program, CompileError> {
    internal::gen_for_program(&c_program)
}
