#[derive(Debug)]
pub struct Token {
    // Line where the token starts.
    pub line_num: usize,
    // Column where the token starts.
    pub col_num: usize,
    // Type of the token.
    pub tp: TokenType,
    // Value of the token. Not all types have values.
    pub val: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    Identitifer,
    Constant,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,
    Int,
    Void,
    Return,
}

impl Token {
    pub fn new(tp: TokenType, val: Option<String>, line_num: usize, col_num: usize) -> Self {
        Self {
            line_num,
            col_num,
            tp,
            val,
        }
    }
}

impl TokenType {
    pub fn name(&self) -> String {
        match self {
            Self::Identitifer => "identifier",
            Self::Constant => "constant",
            Self::OpenParen => "open parenthesis",
            Self::CloseParen => "close parenthesis",
            Self::OpenBrace => "open brace",
            Self::CloseBrace => "close brace",
            Self::Semicolon => "semicolon",
            Self::Int => "int",
            Self::Void => "void",
            Self::Return => "return",
        }
        .into()
    }
}
