mod lexer;

fn main() {
    let source =
        "fn main() { print(\"Hello, world!\"); let x = 1; let y = 2; print(x + y); return; }";
    let mut lexer = lexer::Lexer::new(source.to_string());
    let tokens = lexer.scan_tokens().unwrap();
    for token in tokens {
        println!("{:?}", token);
    }
}
