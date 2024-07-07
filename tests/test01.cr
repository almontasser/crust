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

fn write(fd: u64, s: void*, n: u64): u64 {
    return syscall3(1, fd, s, n);
}

fn main(): u32 {
    puts("Hello, world!\n");
    puts("2+5="); print(2+5); puts("\n");
    puts("2-5="); print(2-5); puts("\n");
    puts("2*5="); print(2*5); puts("\n");
    puts("2.0/5="); print(2.0/5); puts("\n");
    puts("2%5="); print(2%5); puts("\n");
    puts("2==5="); print(2==5); puts("\n");
    puts("2!=5="); print(2!=5); puts("\n");
    puts("2<5="); print(2<5); puts("\n");
    puts("2>5="); print(2>5); puts("\n");
    puts("2<=5="); print(2<=5); puts("\n");
    puts("2>=5="); print(2>=5); puts("\n");


    return 0;
}
