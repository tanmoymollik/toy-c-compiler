#[derive(Debug)]
pub enum Program {
    Function(Function),
}

#[derive(Debug)]
pub struct Function {
    pub name: Identifier,
    pub body: Vec<Instruction>,
}

#[derive(Debug, Clone)]
pub struct Identifier(pub String);

#[derive(Debug)]
pub enum Instruction {
    Mov {
        // dst is always Operand::Stack.
        dst: Operand,
        src: Operand,
    },
    // Operand is always Operand::Stack.
    Unary(UnaryOp, Operand),
    Binary {
        op: BinaryOp,
        // dst is always Operand::Stack.
        dst: Operand,
        src: Operand,
    },
    Idiv(Operand),
    Cdq,
    AllocateStack(usize),
    Ret,
}

#[derive(Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
}

#[derive(Debug, Clone)]
pub enum Operand {
    Imm(i32),
    Reg(Reg),
    Pseudo(Identifier),
    Stack(usize),
}

#[derive(Debug, Clone)]
pub enum Reg {
    Ax,
    Dx,
    R10,
    R11,
}
