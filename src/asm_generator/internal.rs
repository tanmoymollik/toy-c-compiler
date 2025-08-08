use crate::types::c::ast as c_ast;
use crate::types::compile_error::CompileError;
use crate::types::x64::ast as x64_ast;

type CodeGenError<T> = Result<T, CompileError>;
type Instructions = Vec<x64_ast::Instruction>;

pub fn gen_for_program(prog: &c_ast::Program) -> CodeGenError<x64_ast::Program> {
    match prog {
        c_ast::Program::Function(func) => Ok(x64_ast::Program::Function(gen_for_function(func)?)),
    }
}

fn gen_for_function(func: &c_ast::Function) -> CodeGenError<x64_ast::Function> {
    Ok(x64_ast::Function {
        name: x64_ast::Identifier(func.name.0.clone()),
        body: gen_for_statement(&func.body)?,
    })
}

fn gen_for_statement(stmt: &c_ast::Statement) -> CodeGenError<Instructions> {
    match stmt {
        c_ast::Statement::Return(exp) => Ok(gen_for_return(&exp)?),
    }
}

fn gen_for_return(exp: &c_ast::Expression) -> CodeGenError<Instructions> {
    use x64_ast::{Instruction, Operand};
    Ok(vec![
        Instruction::Mov {
            src: gen_for_expression(exp)?,
            dst: Operand::Register,
        },
        Instruction::Ret,
    ])
}

fn gen_for_expression(exp: &c_ast::Expression) -> CodeGenError<x64_ast::Operand> {
    match exp {
        c_ast::Expression::Constant(val) => Ok(x64_ast::Operand::Imm(*val)),
    }
}
