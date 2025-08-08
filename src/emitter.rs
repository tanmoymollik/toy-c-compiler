use crate::types::x64::ast::*;
use std::fs::File;
use std::io::{BufWriter, Error, Write};
use std::path::Path;

mod internal;

pub fn emit(prog: Program, out_file: &Path) -> Result<(), Error> {
    let file = File::create(out_file)?;
    let mut writer = BufWriter::new(file);
    internal::emit_program(&prog, &mut writer)?;
    writer.flush()?;
    Ok(())
}
