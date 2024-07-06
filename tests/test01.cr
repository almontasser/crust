fn write(fd: u64, s: void*, n: u64): u64 {
    return syscall3(1, fd, s, n);
}

fn strlen(s: u8*): u64 {
    let count: u32 = 0;
    while (*s != 0) {
        count = count + 1;
        s = s + 1;
    }
    return count;
}

fn puts(c: u8*) {
    write(1, c, strlen(c));
}

fn main(): u32 {
    puts("Hello, world!\n");
    return 0;
}
