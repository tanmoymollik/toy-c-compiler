use crate::types::c::{ast::Program, token::Token};
use crate::types::compile_error::CompileError;

mod buffer;
mod internal;

use buffer::Buffer;

pub fn parse_program(tokens: Vec<Token>) -> Result<Program, CompileError> {
    let mut buffer = Buffer::new(tokens);
    internal::parse_program(&mut buffer)
}
