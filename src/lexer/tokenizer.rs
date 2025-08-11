use super::buffer::Buffer;
use crate::types::c::token::{Token, TokenType};
use crate::types::compile_error::CompileError;

type LexResult = Result<Option<Token>, CompileError>;

pub fn next_token(buf: &mut Buffer) -> LexResult {
    // Keep looping until we find the first token.
    while ignore_whitespace(buf) || ignore_comment(buf) {}

    for handler in HANDLERS.iter() {
        if let Some(token) = handler(buf)? {
            return Ok(Some(token));
        }
    }
    if let Some(ch) = buf.peek(0) {
        Err(CompileError::new(
            buf.line_num,
            buf.col_num,
            &format!("invalid token {}", ch as char),
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
    if let Some(ch) = buf.peek(0)
        && ch == b'/'
        && let Some(ch) = buf.peek(1)
        && ch == b'*'
    {
        buf.advance_to_next_line();
        ignored = true;
    }
    ignored
}

const HANDLERS: [fn(&mut Buffer) -> LexResult; 9] = [
    match_identifier_or_keyword,
    match_constant,
    match_open_parenthesis,
    match_close_parenthesis,
    match_open_brace,
    match_close_brace,
    match_semicolon,
    match_tilde,
    match_hyphens,
];

fn match_identifier_or_keyword(buf: &mut Buffer) -> LexResult {
    let identifier = match_identifier(buf)?;
    if let Some(Token {
        line_num,
        col_num,
        tp: TokenType::Identitifer,
        val: Some(iden),
    }) = &identifier
        && let Some(token_type) = match_keyword(iden)
    {
        return Ok(Some(Token::new(token_type, None, *line_num, *col_num)));
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
        Some(Token::new(
            TokenType::Identitifer,
            Some(iden),
            line_num,
            col_num,
        ))
    }))
}

fn match_keyword(iden: &str) -> Option<TokenType> {
    match iden {
        "int" => Some(TokenType::Int),
        "void" => Some(TokenType::Void),
        "return" => Some(TokenType::Return),
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
        Some(Token::new(
            TokenType::Constant,
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
        Ok(Some(Token::new(
            TokenType::OpenParen,
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
        Ok(Some(Token::new(
            TokenType::CloseParen,
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
        Ok(Some(Token::new(
            TokenType::OpenBrace,
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
        Ok(Some(Token::new(
            TokenType::CloseBrace,
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
        Ok(Some(Token::new(
            TokenType::Semicolon,
            None,
            line_num,
            col_num,
        )))
    } else {
        Ok(None)
    }
}

fn match_tilde(buf: &mut Buffer) -> LexResult {
    if let Some(ch) = buf.peek(0)
        && ch == b'~'
    {
        let (line_num, col_num) = (buf.line_num, buf.col_num);
        buf.advance();
        Ok(Some(Token::new(TokenType::Tilde, None, line_num, col_num)))
    } else {
        Ok(None)
    }
}

// Matches either `--` (1st priority) or `-`.
fn match_hyphens(buf: &mut Buffer) -> LexResult {
    if let Some(token) = match_double_hyphen(buf)? {
        assert_eq!(token.tp, TokenType::DoubleHyphen);
        Ok(Some(token))
    } else if let Some(token) = match_hyphen(buf)? {
        assert_eq!(token.tp, TokenType::Hyphen);
        Ok(Some(token))
    } else {
        Ok(None)
    }
}

fn match_double_hyphen(buf: &mut Buffer) -> LexResult {
    if let Some(ch) = buf.peek(0)
        && ch == b'-'
        && let Some(ch) = buf.peek(1)
        && ch == b'-'
    {
        let (line_num, col_num) = (buf.line_num, buf.col_num);
        // Twice for two `-`.
        buf.advance();
        buf.advance();
        Ok(Some(Token::new(
            TokenType::DoubleHyphen,
            None,
            line_num,
            col_num,
        )))
    } else {
        Ok(None)
    }
}

fn match_hyphen(buf: &mut Buffer) -> LexResult {
    if let Some(ch) = buf.peek(0)
        && ch == b'-'
    {
        let (line_num, col_num) = (buf.line_num, buf.col_num);
        buf.advance();
        Ok(Some(Token::new(TokenType::Hyphen, None, line_num, col_num)))
    } else {
        Ok(None)
    }
}
