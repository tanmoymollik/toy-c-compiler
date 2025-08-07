use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

// Only support ascii characters.
pub struct Buffer {
    // Line number of the char under head.
    pub line_num: usize,
    // Column number of the char under head.
    pub col_num: usize,

    container: Vec<u8>,
    // Index of the head element.
    head: usize,
}

impl Buffer {
    pub fn load(file_path: &Path) -> io::Result<Self> {
        let mut file = File::open(file_path)?;
        let mut container = Vec::new();
        file.read_to_end(&mut container)?;
        Ok(Buffer {
            line_num: 1,
            col_num: 1,
            container: container,
            head: 0,
        })
    }

    pub fn peek(&self, idx: usize) -> Option<u8> {
        let idx = self.head + idx;
        if idx >= self.container.len() {
            None
        } else {
            Some(self.container[idx])
        }
    }

    pub fn consume(&mut self, r_lim: usize) -> Option<String> {
        if r_lim == 0 || self.head + r_lim > self.container.len() {
            None
        } else {
            let slice = &self.container[self.head..self.head + r_lim];
            // Create return value before to mut borrow below.
            let ret = Some(std::str::from_utf8(slice).unwrap().into());
            for _ in 0..r_lim {
                self.advance();
            }
            ret
        }
    }

    // This method is a no-op when end of buffer is reached and it is no longer possible to
    // advance. [fn@peek] should be called before to make sure it's possible to advance.
    pub fn advance(&mut self) {
        if self.head < self.container.len() {
            let byte = self.container[self.head];
            if byte == b'\n' {
                self.line_num += 1;
                self.col_num = 0;
            } else {
                self.col_num += 1;
            }
            self.head += 1;
        }
    }

    // This method may not advance to the next line if the current line is the last one or if end
    // of buffer has been reached.
    pub fn advance_to_next_line(&mut self) {
        let cur_line = self.line_num;
        while let Some(_) = self.peek(0)
            && cur_line == self.line_num
        {
            self.advance();
        }
    }
}
