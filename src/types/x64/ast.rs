#[derive(Debug)]
pub enum Program {
    Function(Function),
}

#[derive(Debug)]
pub struct Function {
    pub name: Identifier,
    pub body: Vec<Instruction>,
}

#[derive(Debug)]
pub struct Identifier(pub String);

#[derive(Debug)]
pub enum Instruction {
    Mov { src: Operand, dst: Operand },
    Ret,
}

#[derive(Debug)]
pub enum Operand {
    Imm(i32),
    Register,
}
