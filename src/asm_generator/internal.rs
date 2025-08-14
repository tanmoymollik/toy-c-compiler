use std::cmp::max;

use crate::types::tac::ast as tac_ast;
use crate::types::x64::ast as x64_ast;

type Instructions = Vec<x64_ast::Instruction>;

pub fn gen_for_program(prog: &tac_ast::Program) -> x64_ast::Program {
    match prog {
        tac_ast::Program::Function(func) => x64_ast::Program::Function(gen_for_function(func)),
    }
}

fn gen_for_function(func: &tac_ast::Function) -> x64_ast::Function {
    let mut first_pass: Instructions = Vec::new();
    for instruction in &func.body {
        first_pass.append(&mut gen_for_instruction(instruction));
    }

    let mut stack_offset = 0;
    for instruction in &mut first_pass {
        match instruction {
            x64_ast::Instruction::Mov { src, dst } => {
                stack_offset = max(stack_offset, maybe_replace_pseudo_reg(src));
                stack_offset = max(stack_offset, maybe_replace_pseudo_reg(dst));
            }
            x64_ast::Instruction::Unary(_, operand) => {
                stack_offset = max(stack_offset, maybe_replace_pseudo_reg(operand));
            }
            _ => {}
        }
    }

    let mut second_pass: Instructions = Vec::new();
    if stack_offset > 0 {
        second_pass.push(x64_ast::Instruction::AllocateStack(stack_offset));
    }

    for instuction in first_pass {
        match instuction {
            x64_ast::Instruction::Mov { src, dst } => {
                process_mov(src, dst, &mut second_pass);
            }
            other => second_pass.push(other),
        }
    }

    x64_ast::Function {
        name: x64_ast::Identifier(func.name.0.clone()),
        body: second_pass,
    }
}

// Returns the stack offset used to replace the pseudo register.
fn maybe_replace_pseudo_reg(operand: &mut x64_ast::Operand) -> usize {
    if let x64_ast::Operand::Pseudo(iden) = operand {
        let var_id = iden
            .0
            .split('.')
            .last()
            .and_then(|s| s.parse::<usize>().ok());
        if let Some(var_id) = var_id {
            let stack_offset = 4 * (var_id + 1);
            *operand = x64_ast::Operand::Stack(stack_offset);
            return stack_offset;
        }
    }
    0
}

// Fixes mov if invalid (memory address in both src and dst).
// Pushes the valid mov instruction(s) in instructions.
fn process_mov(src: x64_ast::Operand, dst: x64_ast::Operand, instructions: &mut Instructions) {
    if let x64_ast::Operand::Stack(_) = src
        && let x64_ast::Operand::Stack(_) = dst
    {
        instructions.push(x64_ast::Instruction::Mov {
            src,
            dst: x64_ast::Operand::Reg(x64_ast::Reg::R10),
        });
        instructions.push(x64_ast::Instruction::Mov {
            src: x64_ast::Operand::Reg(x64_ast::Reg::R10),
            dst,
        });
    } else {
        instructions.push(x64_ast::Instruction::Mov { src, dst })
    }
}

fn gen_for_instruction(instruction: &tac_ast::Instruction) -> Instructions {
    use x64_ast::{Instruction, Operand, Reg};
    let mut instructions: Instructions = Vec::new();
    match instruction {
        tac_ast::Instruction::Return(val) => {
            let src = gen_for_val(val);
            instructions.push(Instruction::Mov {
                src,
                dst: Operand::Reg(Reg::Ax),
            });
            instructions.push(Instruction::Ret);
        }
        tac_ast::Instruction::Unary { op, src, dst } => {
            let src = gen_for_val(src);
            let dst = gen_for_val(dst);
            let op = gen_for_unary_op(op);
            instructions.push(Instruction::Mov {
                src,
                dst: dst.clone(),
            });
            instructions.push(Instruction::Unary(op, dst))
        }
    }
    instructions
}

fn gen_for_val(val: &tac_ast::Val) -> x64_ast::Operand {
    use x64_ast::{Identifier, Operand};
    match val {
        tac_ast::Val::Constant(i) => Operand::Imm(*i),
        tac_ast::Val::Var(iden) => Operand::Pseudo(Identifier(iden.0.clone())),
    }
}

fn gen_for_unary_op(op: &tac_ast::UnaryOp) -> x64_ast::UnaryOp {
    match op {
        tac_ast::UnaryOp::Complement => x64_ast::UnaryOp::Not,
        tac_ast::UnaryOp::Negate => x64_ast::UnaryOp::Neg,
    }
}
