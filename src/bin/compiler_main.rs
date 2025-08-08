use std::env;
use std::path::Path;

use lib::compile;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    let in_file = Path::new(&args[1]);
    let out_file = Path::new(in_file).with_extension(".s");

    compile(&in_file, &out_file, "-S".into())?;
    Ok(())
}
