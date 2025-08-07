use super::buffer::Buffer;
use crate::types::c_token::{CToken, CTokenType};
use crate::types::compile_error::CompileError;

type LexResult = Result<Option<CToken>, CompileError>;

pub fn next_token(buf: &mut Buffer) -> LexResult {
    // Keep looping until we find the first token.
    while ignore_whitespace(buf) && ignore_comment(buf) {}

    for handler in HANDLERS.iter() {
        if let Some(token) = handler(buf)? {
            return Ok(Some(token));
        }
    }
    if let Some(_) = buf.peek(0) {
        Err(CompileError::new(
            buf.line_num,
            buf.col_num,
            "Invalid Token".into(),
        ))
    } else {
        Ok(None)
    }
}

// Returns whether any whitespace was ignored.
fn ignore_whitespace(buf: &mut Buffer) -> bool {
    let mut ignored = false;
    while let Some(ch) = buf.peek(0)
        && ch.is_ascii_whitespace()
    {
        buf.advance();
        ignored = true;
    }
    ignored
}

// Returns whether any comment was ignored.
fn ignore_comment(buf: &mut Buffer) -> bool {
    let mut ignored = false;
    if let Some(ch) = buf.peek(0)
        && ch == b'/'
        && let Some(ch) = buf.peek(1)
        && ch == b'/'
    {
        buf.advance_to_next_line();
        ignored = true;
    }
    ignored
}

const HANDLERS: [fn(&mut Buffer) -> LexResult; 7] = [
    match_identifier_or_keyword,
    match_constant,
    match_open_parenthesis,
    match_close_parenthesis,
    match_open_brace,
    match_close_brace,
    match_semicolon,
];

fn match_identifier_or_keyword(buf: &mut Buffer) -> LexResult {
    let identifier = match_identifier(buf)?;
    if let Some(CToken {
        line_num,
        col_num,
        tp: CTokenType::Identitifer,
        val: Some(iden),
    }) = &identifier
        && let Some(token_type) = match_keyword(iden)
    {
        return Ok(Some(CToken::new(token_type, None, *line_num, *col_num)));
    }
    Ok(identifier)
}

fn match_identifier(buf: &mut Buffer) -> LexResult {
    let ch = buf.peek(0);
    if let Some(ch) = ch
        && !ch.is_ascii_alphabetic()
        && ch != b'_'
    {
        return Ok(None);
    } else if let None = ch {
        return Ok(None);
    }

    let mut r_idx = 1;
    while let Some(ch) = buf.peek(r_idx)
        && (ch.is_ascii_alphanumeric() || ch == b'_')
    {
        r_idx += 1;
    }
    let (line_num, col_num) = (buf.line_num, buf.col_num);
    Ok(buf.consume(r_idx).and_then(|iden| {
        Some(CToken::new(
            CTokenType::Identitifer,
            Some(iden),
            line_num,
            col_num,
        ))
    }))
}

fn match_keyword(iden: &str) -> Option<CTokenType> {
    match iden {
        "int" => Some(CTokenType::Int),
        "void" => Some(CTokenType::Void),
        "return" => Some(CTokenType::Return),
        _ => None,
    }
}

// Regex pattern: "[0-9]+\b"
fn match_constant(buf: &mut Buffer) -> LexResult {
    // Match digits.
    let mut r_idx = 0;
    while let Some(ch) = buf.peek(r_idx)
        && ch.is_ascii_digit()
    {
        r_idx += 1;
    }
    // Return if no match.
    if r_idx == 0 {
        return Ok(None);
    }
    // Match word boundary.
    if let Some(ch) = buf.peek(r_idx)
        && (ch.is_ascii_alphanumeric() || ch == b'_')
    {
        return Err(CompileError::new(
            buf.line_num,
            buf.col_num,
            "integer must end on word boundary".into(),
        ));
    }

    let (line_num, col_num) = (buf.line_num, buf.col_num);
    Ok(buf.consume(r_idx).and_then(|iden| {
        Some(CToken::new(
            CTokenType::Constant,
            Some(iden),
            line_num,
            col_num,
        ))
    }))
}

fn match_open_parenthesis(buf: &mut Buffer) -> LexResult {
    if let Some(ch) = buf.peek(0)
        && ch == b'('
    {
        let (line_num, col_num) = (buf.line_num, buf.col_num);
        buf.advance();
        Ok(Some(CToken::new(
            CTokenType::OpenParen,
            None,
            line_num,
            col_num,
        )))
    } else {
        Ok(None)
    }
}

fn match_close_parenthesis(buf: &mut Buffer) -> LexResult {
    if let Some(ch) = buf.peek(0)
        && ch == b')'
    {
        let (line_num, col_num) = (buf.line_num, buf.col_num);
        buf.advance();
        Ok(Some(CToken::new(
            CTokenType::CloseParen,
            None,
            line_num,
            col_num,
        )))
    } else {
        Ok(None)
    }
}

fn match_open_brace(buf: &mut Buffer) -> LexResult {
    if let Some(ch) = buf.peek(0)
        && ch == b'{'
    {
        let (line_num, col_num) = (buf.line_num, buf.col_num);
        buf.advance();
        Ok(Some(CToken::new(
            CTokenType::OpenBrace,
            None,
            line_num,
            col_num,
        )))
    } else {
        Ok(None)
    }
}

fn match_close_brace(buf: &mut Buffer) -> LexResult {
    if let Some(ch) = buf.peek(0)
        && ch == b'}'
    {
        let (line_num, col_num) = (buf.line_num, buf.col_num);
        buf.advance();
        Ok(Some(CToken::new(
            CTokenType::CloseBrace,
            None,
            line_num,
            col_num,
        )))
    } else {
        Ok(None)
    }
}

fn match_semicolon(buf: &mut Buffer) -> LexResult {
    if let Some(ch) = buf.peek(0)
        && ch == b';'
    {
        let (line_num, col_num) = (buf.line_num, buf.col_num);
        buf.advance();
        Ok(Some(CToken::new(
            CTokenType::Semicolon,
            None,
            line_num,
            col_num,
        )))
    } else {
        Ok(None)
    }
}
