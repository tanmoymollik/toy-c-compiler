use std::sync::atomic::{AtomicUsize, Ordering};

use crate::types::c::ast as c_ast;
use crate::types::tac::ast as tac_ast;

type Instructions = Vec<tac_ast::Instruction>;

pub fn gen_for_program(prog: &c_ast::Program) -> tac_ast::Program {
    match prog {
        c_ast::Program::Function(func) => tac_ast::Program::Function(gen_for_function(func)),
    }
}

fn gen_for_function(func: &c_ast::Function) -> tac_ast::Function {
    tac_ast::Function {
        name: tac_ast::Identifier(func.name.0.clone()),
        body: gen_for_statement(&func.body),
    }
}

fn gen_for_statement(stmt: &c_ast::Statement) -> Instructions {
    let mut instructions: Instructions = Vec::new();
    match stmt {
        c_ast::Statement::Return(exp) => {
            let var = gen_for_expression(exp, &mut instructions);
            instructions.push(tac_ast::Instruction::Return(var));
            instructions
        }
    }
}

fn gen_for_expression(exp: &c_ast::Expression, instructions: &mut Instructions) -> tac_ast::Val {
    use c_ast::Expression;
    use tac_ast::{Constant, Instruction, Val, Var};
    match exp {
        Expression::Constant(val) => Val::Constant(Constant(*val)),
        Expression::Unary { op, exp } => {
            let src = gen_for_expression(exp, instructions);
            let dst_iden = make_temporary_identifier();
            let dst = Var(dst_iden);
            instructions.push(Instruction::Unary {
                op: gen_for_unary_op(op),
                dst: dst.clone(),
                src,
            });
            Val::Var(dst)
        }
        Expression::Binary { op, left, right } => {
            let src1 = gen_for_expression(left, instructions);
            let src2 = gen_for_expression(right, instructions);
            let dst_iden = make_temporary_identifier();
            let dst = Var(dst_iden);
            instructions.push(Instruction::Binary {
                op: gen_for_binary_op(op),
                dst: dst.clone(),
                src1,
                src2,
            });
            Val::Var(dst)
        }
    }
}

fn gen_for_unary_op(op: &c_ast::UnaryOp) -> tac_ast::UnaryOp {
    match op {
        c_ast::UnaryOp::Complement => tac_ast::UnaryOp::Complement,
        c_ast::UnaryOp::Negate => tac_ast::UnaryOp::Negate,
    }
}

fn gen_for_binary_op(op: &c_ast::BinaryOp) -> tac_ast::BinaryOp {
    match op {
        c_ast::BinaryOp::Add => tac_ast::BinaryOp::Add,
        c_ast::BinaryOp::Subtract => tac_ast::BinaryOp::Subtract,
        c_ast::BinaryOp::Multiply => tac_ast::BinaryOp::Multiply,
        c_ast::BinaryOp::Divide => tac_ast::BinaryOp::Divide,
        c_ast::BinaryOp::Remainder => tac_ast::BinaryOp::Remainder,
    }
}

// Makes a temporary identifier that follows "tmp.{usize}" pattern.
// This helps keep these identifiers distinct as valid C identifiers will never have '.' in them.
fn make_temporary_identifier() -> tac_ast::Identifier {
    static CNTR: AtomicUsize = AtomicUsize::new(0);
    tac_ast::Identifier(format!("tmp.{}", CNTR.fetch_add(1, Ordering::SeqCst)))
}
