use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct CompileError {
    pub line_num: usize,
    pub col_num: usize,
    // `message`` should always start with lowercase letter. Don't add `.` in the end.
    pub message: String,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Error at line: {}:{} - {}",
            self.line_num, self.col_num, self.message
        )
    }
}

impl Error for CompileError {}

impl CompileError {
    pub fn new(line_num: usize, col_num: usize, message: String) -> Self {
        Self {
            line_num,
            col_num,
            message,
        }
    }
}
