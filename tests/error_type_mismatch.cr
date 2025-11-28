// Test: Type mismatch error - invalid operand types
// Expected error: E0402

fn main() {
    // This should cause a type error: comparing int with bool
    println(5 + true);
}
