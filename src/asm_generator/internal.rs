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
            x64_ast::Instruction::Binary { dst, src, .. } => {
                stack_offset = max(stack_offset, maybe_replace_pseudo_reg(dst));
                stack_offset = max(stack_offset, maybe_replace_pseudo_reg(src));
            }
            x64_ast::Instruction::Idiv(operand) => {
                stack_offset = max(stack_offset, maybe_replace_pseudo_reg(operand));
            }
            _ => {}
        }
    }

    // At this point all Operand::Pseudo are converted to Operand::Stack.
    let mut second_pass: Instructions = Vec::new();
    if stack_offset > 0 {
        second_pass.push(x64_ast::Instruction::AllocateStack(stack_offset));
    }

    for instuction in first_pass {
        match instuction {
            x64_ast::Instruction::Mov { src, dst }
                if matches!(dst, x64_ast::Operand::Stack(_))
                    && matches!(src, x64_ast::Operand::Stack(_)) =>
            {
                // Fix mov when invalid (memory address in both src and dst).
                fix_mov(src, dst, &mut second_pass);
            }
            x64_ast::Instruction::Idiv(operand) if matches!(operand, x64_ast::Operand::Imm(_)) => {
                // Fix idiv when invalid (immediate value in src).
                fix_idiv(operand, &mut second_pass);
            }
            x64_ast::Instruction::Binary { op, dst, src }
                if matches!(op, x64_ast::BinaryOp::Add | x64_ast::BinaryOp::Sub)
                    && matches!(dst, x64_ast::Operand::Stack(_))
                    && matches!(src, x64_ast::Operand::Stack(_)) =>
            {
                // Fix add or sub when invalid (memmory address in both src and dst).
                fix_add_or_sub(op, src, dst, &mut second_pass);
            }
            x64_ast::Instruction::Binary { op, dst, src }
                if matches!(op, x64_ast::BinaryOp::Mul)
                    && matches!(dst, x64_ast::Operand::Stack(_)) =>
            {
                // Fix mull when invalid (memory address in dst).
                fix_mul(src, dst, &mut second_pass);
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

fn fix_mov(src: x64_ast::Operand, dst: x64_ast::Operand, instructions: &mut Instructions) {
    let tmp_src = x64_ast::Operand::Reg(x64_ast::Reg::R10);
    instructions.push(x64_ast::Instruction::Mov {
        dst: tmp_src.clone(),
        src,
    });
    instructions.push(x64_ast::Instruction::Mov { dst, src: tmp_src });
}

fn fix_idiv(src: x64_ast::Operand, instructions: &mut Instructions) {
    let dst = x64_ast::Operand::Reg(x64_ast::Reg::R10);
    instructions.push(x64_ast::Instruction::Mov {
        dst: dst.clone(),
        src,
    });
    instructions.push(x64_ast::Instruction::Idiv(dst));
}

fn fix_add_or_sub(
    op: x64_ast::BinaryOp,
    src: x64_ast::Operand,
    dst: x64_ast::Operand,
    instructions: &mut Instructions,
) {
    let tmp_src = x64_ast::Operand::Reg(x64_ast::Reg::R10);
    instructions.push(x64_ast::Instruction::Mov {
        dst: tmp_src.clone(),
        src,
    });
    if matches!(op, x64_ast::BinaryOp::Add) {
        instructions.push(x64_ast::Instruction::Binary {
            op: x64_ast::BinaryOp::Add,
            dst,
            src: tmp_src,
        });
    } else {
        assert!(matches!(op, x64_ast::BinaryOp::Sub));
        instructions.push(x64_ast::Instruction::Binary {
            op: x64_ast::BinaryOp::Sub,
            dst,
            src: tmp_src,
        });
    }
}

fn fix_mul(src: x64_ast::Operand, dst: x64_ast::Operand, instructions: &mut Instructions) {
    let tmp_dst = x64_ast::Operand::Reg(x64_ast::Reg::R11);
    instructions.push(x64_ast::Instruction::Mov {
        dst: tmp_dst.clone(),
        src: dst.clone(),
    });
    instructions.push(x64_ast::Instruction::Binary {
        op: x64_ast::BinaryOp::Mul,
        dst: tmp_dst.clone(),
        src,
    });
    instructions.push(x64_ast::Instruction::Mov {
        dst: dst,
        src: tmp_dst,
    });
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
            let dst = gen_for_var(dst);
            let op = gen_for_unary_op(op);
            instructions.push(Instruction::Mov {
                src,
                dst: dst.clone(),
            });
            instructions.push(Instruction::Unary(op, dst));
        }
        tac_ast::Instruction::Binary {
            op,
            dst,
            src1,
            src2,
        } => {
            return gen_for_binary_instruction(op, dst, src1, src2);
        }
    }
    instructions
}

fn gen_for_val(val: &tac_ast::Val) -> x64_ast::Operand {
    use tac_ast::{Constant, Val};
    match val {
        Val::Constant(Constant(i)) => x64_ast::Operand::Imm(*i),
        Val::Var(var) => gen_for_var(var),
    }
}

fn gen_for_var(var: &tac_ast::Var) -> x64_ast::Operand {
    let iden = &var.0;
    x64_ast::Operand::Pseudo(x64_ast::Identifier(iden.0.clone()))
}

fn gen_for_unary_op(op: &tac_ast::UnaryOp) -> x64_ast::UnaryOp {
    match op {
        tac_ast::UnaryOp::Complement => x64_ast::UnaryOp::Not,
        tac_ast::UnaryOp::Negate => x64_ast::UnaryOp::Neg,
    }
}

fn gen_for_binary_instruction(
    op: &tac_ast::BinaryOp,
    dst: &tac_ast::Var,
    src1: &tac_ast::Val,
    src2: &tac_ast::Val,
) -> Instructions {
    let dst = gen_for_var(dst);
    let src1 = gen_for_val(src1);
    let src2 = gen_for_val(src2);

    if matches!(op, tac_ast::BinaryOp::Divide) {
        return gen_for_binary_divide(false, dst, src1, src2);
    } else if matches!(op, tac_ast::BinaryOp::Remainder) {
        return gen_for_binary_divide(true, dst, src1, src2);
    }

    let op = match op {
        tac_ast::BinaryOp::Add => x64_ast::BinaryOp::Add,
        tac_ast::BinaryOp::Subtract => x64_ast::BinaryOp::Sub,
        tac_ast::BinaryOp::Multiply => x64_ast::BinaryOp::Mul,
        _ => unreachable!(),
    };

    let mut instructions: Instructions = Vec::new();
    instructions.push(x64_ast::Instruction::Mov {
        dst: dst.clone(),
        src: src1,
    });
    instructions.push(x64_ast::Instruction::Binary { op, dst, src: src2 });
    instructions
}

fn gen_for_binary_divide(
    rem: bool,
    dst: x64_ast::Operand,
    src1: x64_ast::Operand,
    src2: x64_ast::Operand,
) -> Instructions {
    use x64_ast::Instruction;
    use x64_ast::Operand;
    use x64_ast::Reg;

    let mut instructions: Instructions = Vec::new();
    instructions.push(Instruction::Mov {
        dst: Operand::Reg(Reg::Ax),
        src: src1,
    });
    instructions.push(Instruction::Cdq);
    instructions.push(Instruction::Idiv(src2));
    instructions.push(Instruction::Mov {
        dst,
        src: if rem {
            Operand::Reg(Reg::Dx)
        } else {
            Operand::Reg(Reg::Ax)
        },
    });
    instructions
}
