use crate::types::c::ast::*;
use crate::types::c::token::{Token, TokenType};
use crate::types::compile_error::CompileError;

pub struct Buffer {
    container: Vec<Token>,
    cur_idx: usize,
}

impl Buffer {
    pub fn new(tokens: Vec<Token>) -> Self {
        Buffer {
            container: tokens,
            cur_idx: 0,
        }
    }

    fn peek(&self, idx: usize) -> Option<&Token> {
        let idx = self.cur_idx + idx;
        if idx >= self.container.len() {
            None
        } else {
            Some(&self.container[idx])
        }
    }

    fn advance(&mut self, step: usize) -> bool {
        if self.cur_idx + step > self.container.len() {
            false
        } else {
            self.cur_idx += step;
            true
        }
    }

    fn last_line_and_col(&self) -> (usize, usize) {
        if let Some(token) = self.container.last() {
            (token.line_num, token.col_num)
        } else {
            (0, 0)
        }
    }
}

type ParseResult<T> = Result<T, CompileError>;

pub fn parse_program(buf: &mut Buffer) -> Result<Program, CompileError> {
    let function = parse_function(buf)?;
    if let Some(Token {
        line_num, col_num, ..
    }) = buf.peek(0)
    {
        return Err(CompileError::new(
            *line_num,
            *col_num,
            "no more tokens expected".into(),
        ));
    }
    Ok(Program::Function(function))
}

fn parse_function(buf: &mut Buffer) -> ParseResult<Function> {
    expect_ctoken_type(buf, TokenType::Int)?;
    let name = parse_identifier(buf)?;
    expect_ctoken_type(buf, TokenType::OpenParen)?;
    expect_ctoken_type(buf, TokenType::Void)?;
    expect_ctoken_type(buf, TokenType::CloseParen)?;
    expect_ctoken_type(buf, TokenType::OpenBrace)?;
    let body = parse_statement(buf)?;
    expect_ctoken_type(buf, TokenType::CloseBrace)?;
    Ok(Function { name, body })
}

fn parse_statement(buf: &mut Buffer) -> ParseResult<Statement> {
    let exp = parse_return(buf)?;
    Ok(Statement::Return(exp))
}

fn parse_return(buf: &mut Buffer) -> ParseResult<Expression> {
    expect_ctoken_type(buf, TokenType::Return)?;
    let exp = parse_expression(buf)?;
    expect_ctoken_type(buf, TokenType::Semicolon)?;
    Ok(exp)
}

fn parse_expression(buf: &mut Buffer) -> ParseResult<Expression> {
    match buf.peek(0) {
        Some(token) if token.tp == TokenType::Constant => {
            // Store the result in order to advance buffer.
            let res = token
                .val
                .as_ref()
                .and_then(|s| s.parse::<i32>().ok())
                .and_then(|val| Some(Expression::Constant(val)))
                .ok_or(CompileError::new(
                    token.line_num,
                    token.col_num,
                    "integer out of range or invalid",
                ));
            buf.advance(1);
            res
        }
        Some(token) if token.tp == TokenType::Tilde || token.tp == TokenType::Hyphen => {
            let op = parse_unary_op(buf)?;
            let exp = parse_expression(buf)?;
            Ok(Expression::Unary {
                op,
                exp: Box::new(exp),
            })
        }
        Some(token) if token.tp == TokenType::OpenParen => {
            expect_ctoken_type(buf, TokenType::OpenParen)?;
            let exp = parse_expression(buf)?;
            expect_ctoken_type(buf, TokenType::CloseParen)?;
            Ok(exp)
        }
        Some(Token {
            line_num, col_num, ..
        }) => Err(CompileError::new(
            *line_num,
            *col_num,
            "Malformed expression",
        )),
        None => Err(out_of_token_error(buf, "expected an expression")),
    }
}

fn parse_identifier(buf: &mut Buffer) -> ParseResult<Identifier> {
    if let Token {
        val: Some(iden), ..
    } = expect_ctoken_type(buf, TokenType::Identitifer)?
    {
        return Ok(Identifier(iden.into()));
    }
    unreachable!("Identifier should always have a value.");
}

fn parse_unary_op(buf: &mut Buffer) -> ParseResult<UnaryOp> {
    match buf.peek(0) {
        Some(Token {
            tp: TokenType::Tilde,
            ..
        }) => {
            buf.advance(1);
            Ok(UnaryOp::Complement)
        }
        Some(Token {
            tp: TokenType::Hyphen,
            ..
        }) => {
            buf.advance(1);
            Ok(UnaryOp::Negate)
        }
        Some(Token {
            line_num, col_num, ..
        }) => Err(CompileError::new(
            *line_num,
            *col_num,
            "expected an unary operator",
        )),
        None => Err(out_of_token_error(buf, "")),
    }
}

// Returns the token if the next token matches token_type.
fn expect_ctoken_type(buf: &mut Buffer, token_type: TokenType) -> ParseResult<Token> {
    if let Some(Token {
        line_num,
        col_num,
        tp,
        val,
    }) = buf.peek(0)
    {
        return if *tp == token_type {
            // Create return token out of reference.
            let token = Token::new(*tp, val.clone(), *line_num, *col_num);
            buf.advance(1);
            Ok(token)
        } else {
            Err(CompileError::new(
                *line_num,
                *col_num,
                &format!("expected {}", token_type.name()),
            ))
        };
    }
    // Out of token error.
    Err(out_of_token_error(
        buf,
        &format!("expected {}", token_type.name()),
    ))
}

fn out_of_token_error(buf: &Buffer, message: &str) -> CompileError {
    let (line_num, col_num) = buf.last_line_and_col();
    CompileError::new(line_num, col_num, message.into())
}
