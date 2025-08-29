use super::buffer::Buffer;
use crate::types::c::ast::*;
use crate::types::c::token::{Token, TokenType};
use crate::types::compile_error::CompileError;

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

fn parse_identifier(buf: &mut Buffer) -> ParseResult<Identifier> {
    if let Token {
        val: Some(iden), ..
    } = expect_ctoken_type(buf, TokenType::Identitifer)?
    {
        return Ok(Identifier(iden.into()));
    }
    unreachable!("Identifier should always have a value.");
}

fn parse_statement(buf: &mut Buffer) -> ParseResult<Statement> {
    let exp = parse_return(buf)?;
    Ok(Statement::Return(exp))
}

fn parse_return(buf: &mut Buffer) -> ParseResult<Expression> {
    expect_ctoken_type(buf, TokenType::Return)?;
    let exp = parse_expression(buf, 0)?;
    expect_ctoken_type(buf, TokenType::Semicolon)?;
    Ok(exp)
}

fn parse_expression(buf: &mut Buffer, pr: usize) -> ParseResult<Expression> {
    let mut left = parse_factor(buf)?;
    while let Some(token) = buf.peek(0)
        && is_binary_op(&token.tp)
        && precedence(&token.tp) >= pr
    {
        let next_pr = precedence(&token.tp) + 1;
        let op = parse_binary_op(buf)?;
        let right = parse_expression(buf, next_pr)?;
        left = Expression::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
        };
    }
    Ok(left)
}

fn precedence(op: &TokenType) -> usize {
    match op {
        TokenType::Plus => 0,
        TokenType::Hyphen => 0,
        TokenType::Asterisk => 1,
        TokenType::ForwardSlash => 1,
        TokenType::Percent => 1,
        _ => unreachable!("Invalid binary op"),
    }
}

fn parse_factor(buf: &mut Buffer) -> ParseResult<Expression> {
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
        Some(token) if is_unary_op(&token.tp) => {
            let op = parse_unary_op(buf)?;
            let exp = parse_factor(buf)?;
            Ok(Expression::Unary {
                op,
                exp: Box::new(exp),
            })
        }
        Some(token) if token.tp == TokenType::OpenParen => {
            expect_ctoken_type(buf, TokenType::OpenParen)?;
            let exp = parse_expression(buf, 0)?;
            expect_ctoken_type(buf, TokenType::CloseParen)?;
            Ok(exp)
        }
        Some(Token {
            line_num, col_num, ..
        }) => Err(CompileError::new(*line_num, *col_num, "Malformed factor")),
        None => Err(out_of_token_error(buf, "expected an expression")),
    }
}

fn is_unary_op(tp: &TokenType) -> bool {
    match tp {
        TokenType::Tilde => true,
        TokenType::Hyphen => true,
        _ => false,
    }
}

fn is_binary_op(tp: &TokenType) -> bool {
    match tp {
        TokenType::Hyphen => true,
        TokenType::Plus => true,
        TokenType::Asterisk => true,
        TokenType::ForwardSlash => true,
        TokenType::Percent => true,
        _ => false,
    }
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

fn parse_binary_op(buf: &mut Buffer) -> ParseResult<BinaryOp> {
    match buf.peek(0) {
        Some(Token {
            tp: TokenType::Hyphen,
            ..
        }) => {
            buf.advance(1);
            Ok(BinaryOp::Subtract)
        }
        Some(Token {
            tp: TokenType::Plus,
            ..
        }) => {
            buf.advance(1);
            Ok(BinaryOp::Add)
        }
        Some(Token {
            tp: TokenType::Asterisk,
            ..
        }) => {
            buf.advance(1);
            Ok(BinaryOp::Multiply)
        }
        Some(Token {
            tp: TokenType::ForwardSlash,
            ..
        }) => {
            buf.advance(1);
            Ok(BinaryOp::Divide)
        }
        Some(Token {
            tp: TokenType::Percent,
            ..
        }) => {
            buf.advance(1);
            Ok(BinaryOp::Remainder)
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
