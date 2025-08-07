use crate::types::{c_ast::Program, c_token::CToken, compile_error::CompileError};

mod internal;

use internal::Buffer;

pub fn parse_program(tokens: Vec<CToken>) -> Result<Program, CompileError> {
    let mut buffer = Buffer::new(tokens);
    internal::parse_program(&mut buffer)
}
