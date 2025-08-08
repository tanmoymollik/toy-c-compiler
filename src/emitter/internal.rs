use crate::types::x64::ast::*;
use std::io::{Error, Write};

type EmitError = Result<(), Error>;

pub fn emit_program(prog: &Program, writer: &mut impl Write) -> EmitError {
    match prog {
        Program::Function(func) => emit_function(&func, writer)?,
    }
    if cfg!(feature = "linux") {
        writeln!(writer, r#".section .note.GNU-stack,"",@progbits"#)?;
    }
    Ok(())
}

fn emit_function(func: &Function, writer: &mut impl Write) -> EmitError {
    let name = if cfg!(feature = "linux") {
        &func.name.0
    } else {
        // macos
        &format!("_{}", func.name.0)
    };
    // TODO(): Write a proper indenter.
    writeln!(writer, "  .global {}\n{}:", name, name)?;
    for ins in &func.body {
        writeln!(writer, "{}", emit_instruction(ins))?;
    }
    Ok(())
}

fn emit_instruction(ins: &Instruction) -> String {
    match ins {
        Instruction::Mov { src, dst } => {
            format!("mov {}, {}", emit_operand(dst), emit_operand(src))
        }
        Instruction::Ret => "ret".into(),
    }
}

fn emit_operand(op: &Operand) -> String {
    match op {
        Operand::Imm(val) => format!("{val}"),
        Operand::Register => "eax".into(),
    }
}
