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
    Return(Val),
    Unary {
        op: UnaryOp,
        dst: Var,
        src: Val,
    },
    Binary {
        op: BinaryOp,
        dst: Var,
        src1: Val,
        src2: Val,
    },
}

#[derive(Debug, Clone)]
pub enum Val {
    Constant(Constant),
    Var(Var),
}

#[derive(Debug, Clone)]
pub struct Constant(pub i32);

#[derive(Debug, Clone)]
pub struct Var(pub Identifier);

#[derive(Debug)]
pub enum UnaryOp {
    Complement,
    Negate,
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
}
