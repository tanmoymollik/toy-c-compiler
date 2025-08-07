use std::env;
use std::path::Path;
use std::process::Command;

use lib::compile;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    let stage = &args[1];
    let file_name = &args[2];
    let file_path = Path::new(file_name).with_extension("");
    let file_base_name = file_path.to_str().unwrap();

    // Preprocessing step
    Command::new("sh")
        .arg("-c")
        .arg(format!("gcc -E -P {file_name} -o {file_base_name}.i"))
        .output()?;

    // Compilation step
    let result = compile(
        Path::new(file_name),
        Path::new(&format!("{file_base_name}.s")),
        stage.into(),
    )?;
    Command::new("sh")
        .arg("-c")
        .arg(format!("rm {file_base_name}.i"))
        .output()?;

    // Code emission step
    Command::new("sh")
        .arg("-c")
        .arg(format!("gcc {file_base_name}.s -o {file_base_name}.o"))
        .output()?;
    Command::new("sh")
        .arg("-c")
        .arg(format!("rm {file_base_name}.s"))
        .output()?;

    Ok(())
}
