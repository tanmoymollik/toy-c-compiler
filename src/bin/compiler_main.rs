use std::env;
use std::path::Path;
use std::process::Command;

use lib::compile;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    let input_file = &args[1];
    let output_file = &args[2];

    let result = compile(Path::new(input_file), Path::new(output_file), "".into())?;
    // Stub
    Command::new("sh")
        .arg("-c")
        .arg(format!("cp stub.s {output_file}"))
        .output()?;

    Ok(())
}
