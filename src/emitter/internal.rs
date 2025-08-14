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

    writeln!(writer, "section .text")?;
    writeln!(writer, "{}global {}\n", emit_indent(1), name)?;
    writeln!(writer, "{}:", name)?;
    // prologue
    writeln!(writer, "{}push rbp", emit_indent(1))?;
    writeln!(writer, "{}mov rbp, rsp", emit_indent(1))?;

    for ins in &func.body {
        emit_instruction(ins, writer)?;
    }
    Ok(())
}

fn emit_instruction(ins: &Instruction, writer: &mut impl Write) -> EmitError {
    match ins {
        Instruction::AllocateStack(offset) => {
            writeln!(writer, "{}sub rsp, {offset}", emit_indent(1))?
        }
        Instruction::Mov { src, dst } => writeln!(
            writer,
            "{}mov {}, {}",
            emit_indent(1),
            emit_operand(dst),
            emit_operand(src)
        )?,
        Instruction::Ret => {
            writeln!(writer, "{}leave", emit_indent(1))?;
            writeln!(writer, "{}ret", emit_indent(1))?;
        }
        Instruction::Unary(op, operand) => writeln!(
            writer,
            "{}{} {}",
            emit_indent(1),
            emit_unary_op(op),
            emit_operand(operand)
        )?,
    }
    Ok(())
}

fn emit_unary_op(op: &UnaryOp) -> String {
    match op {
        UnaryOp::Neg => "neg".into(),
        UnaryOp::Not => "not".into(),
    }
}

fn emit_operand(operand: &Operand) -> String {
    match operand {
        Operand::Imm(val) => format!("{val}"),
        Operand::Reg(reg) => emit_register(reg),
        Operand::Stack(offset) => format!("dword [rbp - {offset}]"),
        _ => unreachable!(),
    }
}

fn emit_register(reg: &Reg) -> String {
    match reg {
        Reg::Ax => "eax".into(),
        Reg::R10 => "r10d".into(),
    }
}

fn emit_indent(lvl: u8) -> String {
    // 2 space indent.
    std::iter::repeat("  ").take(lvl as usize).collect()
}
