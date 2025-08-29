use crate::types::c::token::Token;

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

    pub fn peek(&self, idx: usize) -> Option<&Token> {
        let idx = self.cur_idx + idx;
        if idx >= self.container.len() {
            None
        } else {
            Some(&self.container[idx])
        }
    }

    pub fn advance(&mut self, step: usize) -> bool {
        if self.cur_idx + step > self.container.len() {
            false
        } else {
            self.cur_idx += step;
            true
        }
    }

    pub fn last_line_and_col(&self) -> (usize, usize) {
        if let Some(token) = self.container.last() {
            (token.line_num, token.col_num)
        } else {
            (0, 0)
        }
    }
}
