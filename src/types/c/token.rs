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
    // Chapter 1 tokens.
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
    // Chapter 2 tokens.
    Tilde,
    Hyphen,
    DoubleHyphen,
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
            Self::Identitifer => "an identifier",
            Self::Constant => "a constant",
            Self::OpenParen => "'('",
            Self::CloseParen => "')'",
            Self::OpenBrace => "'{'",
            Self::CloseBrace => "'}'",
            Self::Semicolon => "';'",
            Self::Int => "keyword int",
            Self::Void => "keyword void",
            Self::Return => "keyword return",
            Self::Tilde => "'~'",
            Self::Hyphen => "'-'",
            Self::DoubleHyphen => "'--'",
        }
        .into()
    }
}
