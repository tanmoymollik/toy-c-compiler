pub trait CAstNode {
    fn len(&self) -> usize;
}

#[derive(Debug)]
pub enum Program {
    Function(FunctionDef),
}

impl CAstNode for Program {
    fn len(&self) -> usize {
        match self {
            Self::Function(function_def) => function_def.len(),
        }
    }
}

#[derive(Debug)]
pub struct FunctionDef {
    pub name: Identifier,
    pub body: Statement,
}

impl CAstNode for FunctionDef {
    fn len(&self) -> usize {
        self.name.len() + self.body.len()
    }
}

#[derive(Debug)]
pub struct Identifier(pub String);

impl CAstNode for Identifier {
    fn len(&self) -> usize {
        1
    }
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
}

impl CAstNode for Statement {
    fn len(&self) -> usize {
        match self {
            // "return" (1 token) + exp.len() + ";" (1 token)
            Self::Return(exp) => 2 + exp.len(),
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Constant(i32),
}

impl CAstNode for Expression {
    fn len(&self) -> usize {
        match self {
            Self::Constant(_) => 1,
        }
    }
}
