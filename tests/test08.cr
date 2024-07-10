fn main() {
    let i = 0;
    while (i < 10) {
        defer print(9999);
        print(i);
        i = i + 1;
        if (i == 5) {
            defer print(i);
            continue;
        }
    }
}