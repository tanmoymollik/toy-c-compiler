use std::error::Error;
use std::path::Path;

mod buffer;
mod tokenizer;

use crate::types::c_token::CToken;
use buffer::Buffer;

pub fn tokenize(file_path: &Path) -> Result<Vec<CToken>, Box<dyn Error>> {
    let mut buffer = Buffer::load(file_path)?;
    let mut tokens = Vec::new();
    while let Some(token) = tokenizer::next_token(&mut buffer)? {
        tokens.push(token);
    }
    Ok(tokens)
}
