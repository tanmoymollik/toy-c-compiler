use std::error::Error;
use std::path::Path;

mod asm_generator;
mod emitter;
mod lexer;
mod parser;
mod tac_generator;
mod types;

pub fn compile(
    in_file_path: &Path,
    out_file_path: &Path,
    stage: String,
) -> Result<(), Box<dyn Error>> {
    let tokens = lexer::tokenize(in_file_path)?;
    if stage == "--lex" {
        return Ok(());
    }

    let c_program = parser::parse_program(tokens)?;
    if stage == "--parse" {
        return Ok(());
    }

    let tac_program = tac_generator::generate(c_program);
    if stage == "--tacky" {
        return Ok(());
    }

    let x64_program = asm_generator::generate(tac_program);
    if stage == "--codegen" {
        return Ok(());
    }

    emitter::emit(x64_program, out_file_path)?;
    Ok(())
}
