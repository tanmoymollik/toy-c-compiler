use crate::types::c_ast::*;
use crate::types::c_token::{CToken, CTokenType};
use crate::types::compile_error::CompileError;

pub struct Buffer {
    container: Vec<CToken>,
    cur_idx: usize,
}

impl Buffer {
    pub fn new(tokens: Vec<CToken>) -> Self {
        Buffer {
            container: tokens,
            cur_idx: 0,
        }
    }

    fn peek(&self, idx: usize) -> Option<&CToken> {
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
    if let Some(CToken {
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

fn parse_function(buf: &mut Buffer) -> ParseResult<FunctionDef> {
    expect_ctoken_type(buf, CTokenType::Int)?;
    let name = parse_identifier(buf)?;
    expect_ctoken_type(buf, CTokenType::OpenParen)?;
    expect_ctoken_type(buf, CTokenType::Void)?;
    expect_ctoken_type(buf, CTokenType::CloseParen)?;
    expect_ctoken_type(buf, CTokenType::OpenBrace)?;
    let body = parse_statement(buf)?;
    expect_ctoken_type(buf, CTokenType::CloseBrace)?;
    Ok(FunctionDef { name, body })
}

fn parse_statement(buf: &mut Buffer) -> ParseResult<Statement> {
    let exp = parse_return(buf)?;
    Ok(Statement::Return(exp))
}

fn parse_return(buf: &mut Buffer) -> ParseResult<Expression> {
    expect_ctoken_type(buf, CTokenType::Return)?;
    let exp = parse_expression(buf)?;
    expect_ctoken_type(buf, CTokenType::Semicolon)?;
    Ok(exp)
}

fn parse_expression(buf: &mut Buffer) -> ParseResult<Expression> {
    if let CToken {
        line_num,
        col_num,
        val: Some(val),
        ..
    } = expect_ctoken_type(buf, CTokenType::Constant)?
    {
        return val
            .parse::<i32>()
            .and_then(|val| Ok(Expression::Constant(val)))
            .map_err(|_| CompileError {
                line_num,
                col_num,
                message: "integer out of range or invalid".into(),
            });
    }
    unreachable!("Constant should always have a value.");
}

fn parse_identifier(buf: &mut Buffer) -> ParseResult<Identifier> {
    if let CToken {
        val: Some(iden), ..
    } = expect_ctoken_type(buf, CTokenType::Identitifer)?
    {
        return Ok(Identifier(iden.into()));
    }
    unreachable!("Identifier should always have a value.");
}

// Returns CToken::val if the next token matches token_type.
fn expect_ctoken_type(buf: &mut Buffer, token_type: CTokenType) -> ParseResult<CToken> {
    if let Some(CToken {
        line_num,
        col_num,
        tp,
        val,
    }) = buf.peek(0)
    {
        return if *tp == token_type {
            // Create return token out of reference.
            let token = CToken::new(*tp, val.clone(), *line_num, *col_num);
            buf.advance(1);
            Ok(token)
        } else {
            Err(CompileError {
                line_num: *line_num,
                col_num: *col_num,
                message: format!("expected {}", token_type.name()),
            })
        };
    }
    // Out of token error.
    let (line_num, col_num) = buf.last_line_and_col();
    Err(CompileError {
        line_num,
        col_num,
        message: format!("expected {}", token_type.name()),
    })
}
