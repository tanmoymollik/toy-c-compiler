use crate::types::c::ast as c_ast;
use crate::types::tac::ast as tac_ast;

mod internal;

pub fn generate(c_program: c_ast::Program) -> tac_ast::Program {
    internal::gen_for_program(&c_program)
}
