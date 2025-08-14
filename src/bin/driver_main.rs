use std::env;
use std::path::Path;
use std::process::Command;

use lib;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    let stage = if args.len() > 2 { &args[1] } else { "" };
    let file_name = if args.len() > 2 { &args[2] } else { &args[1] };
    let file_base_name: String = Path::new(file_name)
        .with_extension("")
        .to_str()
        .unwrap()
        .into();

    // Preprocessing step.
    Command::new("sh")
        .arg("-c")
        .arg(format!("gcc -E -P {file_name} -o {file_base_name}.i"))
        .output()?;

    // Compilation step.
    // Store the result to allow for cleanup of the preprocessed file.
    let ret = lib::compile(
        Path::new(file_name),
        Path::new(&format!("{file_base_name}.s")),
        stage.into(),
    );
    Command::new("sh")
        .arg("-c")
        .arg(format!("rm {file_base_name}.i"))
        .output()?;
    // Propagate error.
    ret?;
    if stage == "-S" {
        return Ok(());
    }

    // Assembly and link step.
    let nasm_command = if cfg!(feature = "linux") {
        "nasm -f elf64"
    } else {
        // macos
        "nasm -f macho64"
    };
    Command::new("sh")
        .arg("-c")
        .arg(format!(
            "{nasm_command} {file_base_name}.s -o {file_base_name}.o"
        ))
        .output()?;
    Command::new("sh")
        .arg("-c")
        .arg(format!("gcc {file_base_name}.o -o {file_base_name}"))
        .output()?;
    Command::new("sh")
        .arg("-c")
        .arg(format!("rm {file_base_name}.s {file_base_name}.o"))
        .output()?;

    Ok(())
}
