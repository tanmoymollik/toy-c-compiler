use std::error::Error;
use std::path::Path;
use std::process::Command;

mod lexer;
mod parser;
mod types;

pub fn test() {}

/// [fn@test]
pub fn compile(
    input_file_path: &Path,
    output_file_path: &Path,
    stage: String,
) -> Result<(), Box<dyn Error>> {
    let tokens = lexer::tokenize(input_file_path)?;
    if stage == "--lex" {
        return Ok(());
    }

    let program = parser::parse_program(tokens)?;
    if stage == "--parse" {
        return Ok(());
    }

    if stage == "--codegen" {
        return Ok(());
    }

    if let Some(output_file) = output_file_path.to_str() {
        Command::new("sh")
            .arg("-c")
            .arg(format!("cp stub.s {output_file}"))
            .output()?;
    }
    Ok(())
}
