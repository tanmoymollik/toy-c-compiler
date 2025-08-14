use crate::types::tac::ast as tac_ast;
use crate::types::x64::ast as x64_ast;

mod internal;

pub fn generate(tac_program: tac_ast::Program) -> x64_ast::Program {
    internal::gen_for_program(&tac_program)
}
