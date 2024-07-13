import "std/common.cr"

fn main() {
    // match test
    let i = 10;
    match (i) {
        1: {
            puts("1");
        }
        2: {
            puts("2");
        }
        10: puts("10");
        15: puts("15");
        default: puts("default");
    }
}