use codecrafters_grep::search;
use std::env;
use std::io;
use std::io::Read;
use std::process;

// Usage: echo <input_text> | your_program.sh -E <pattern>
fn main() {
    if env::args().nth(1).unwrap() != "-E" {
        println!("Expected first argument to be '-E'");
        process::exit(1);
    }

    let pattern = env::args().nth(2).unwrap();
    let mut input_line = String::new();

    io::stdin().read_to_string(&mut input_line).unwrap();

    let filtered = search(&input_line, &pattern);
    if filtered.is_empty() {
        process::exit(1);
    }
    process::exit(0);
}
