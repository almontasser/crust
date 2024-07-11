// This should really be a constant, but we only allow integers...
let null: void*; // Zero initialized by default.


///////////////////////////////////////////////////////////////////////////////
// Syscalls

const SYS_READ = 0;
const SYS_WRITE = 1;
const SYS_OPEN = 2;
const SYS_CLOSE = 3;
const SYS_LSEEK = 8;
const SYS_MMAP = 9;
const SYS_EXECVE = 59;
const SYS_EXIT = 60;
const SYS_WAIT4 = 61;
const SYS_OPENAT = 257;

const PROT_READ = 0x1;
const PROT_WRITE = 0x2;
const PROT_EXEC = 0x4;
const PROT_NONE = 0x0;

const MAP_SHARED = 0x01;
const MAP_PRIVATE = 0x02;
const MAP_SHARED_VALIDATE = 0x03;

let MAP_FAILED: void* = -1;

fn write(fd: u64, s: void*, n: u64): u64 {
    return syscall3(SYS_WRITE, fd, s, n);
}

fn exit(status: u64): u64 {
    return syscall1(SYS_EXIT, status);
}

fn read(fd: u64, s: void*, n: u64): u64 {
    return syscall3(SYS_READ, fd, s, n);
}

fn open(path: u8*, flags: u64, mode: u64): u64 {
    return syscall3(SYS_OPEN, path, flags, mode);
}

fn close(fd: u64): u64 {
    return syscall1(SYS_CLOSE, fd);
}

fn openat(fd: u64, path: u8*, flags: u64, mode: u64): u64 {
    return syscall4(SYS_OPENAT, fd, path, flags, mode);
}

// fork() is a builtin because of OS-specific semantics.

fn wait(status: u64*): u64 {
    return syscall4(SYS_WAIT4, -1, status, 0, 0);
}

fn lseek(fd: u64, offset: u64, whence: u64): u64 {
    return syscall3(SYS_LSEEK, fd, offset, whence);
}

fn mmap(addr: void*, len: u64, prot: u64, flags: u64, fd: u64, offset: u64): void* {
    return syscall6(SYS_MMAP, addr, len, prot, flags, fd, offset);
}

fn execve(filename: u8*, argv: u8**, envp: u8**): u64 {
    return syscall3(SYS_EXECVE, filename, argv, envp);
}

///////////////////////////////////////////////////////////////////////////////
// Strings

fn strlen(s: u8 *): u64 {
    let count: u64 = 0;
    while (*s != 0) {
        count = count + 1;
        s = s + 1;
    }
    return count;
}

fn strcpy(dst: u8 *, src: u8 *) {
    while (*src != 0) {
        *dst = *src;
        dst = dst + 1;
        src = src + 1;
    }
    *dst = '\0';
}

fn strcat(dst: u8 *, src: u8 *) {
    while (*dst != 0) {
        dst = dst + 1;
    }
    while (*src != 0) {
        *dst = *src;
        dst = dst + 1;
        src = src + 1;
    }
    *dst = '\0';
}

fn strstartswith(s: u8 *, prefix: u8 *): bool {
    while (*prefix) {
        if (*s != *prefix)
            return false;
        ++s;
        ++prefix;
    }
    return true;
}

fn streq(s1: u8 *, s2: u8 *): bool {
    while (*s1 != 0 && *s2 != 0) {
        if (*s1 != *s2)
            return false;
        s1 = s1 + 1;
        s2 = s2 + 1;
    }
    return *s1 == *s2;
}

fn strrev(s: u8 *) {
    let len: u64 = strlen(s);
    let i: u64 = 0;
    let j: u64 = len - 1;
    while (i < j) {
        let tmp: u8 = s[i];
        s[i] = s[j];
        s[j] = tmp;
        i = i + 1;
        j = j - 1;
    }
}

fn atoi_end(s: u8 *, end: u8**): u64 {
    let i: u64 = 0;
    let _sign: u64 = 1;
    if (*s == '-') {
        _sign = -1;
        s = s + 1;
    }
    while (*s >= '0' && *s <= '9') {
        i = i * 10 + (*s - '0');
        s = s + 1;
    }
    *end = s;
    return i * _sign;
}

fn atoi(s: u8 *): u64 {
    let tmp: u8*;
    return atoi_end(s, &tmp);
}

fn find_extension_start(s: u8*): u64 {
    let len = strlen(s);
    let i = len - 1;
    while (i >= 0) {
        if (s[i] == '.')
        return i;
        if (s[i] == '/')
        return len;
        --i;
    }
    return len;
}

fn replace_extension(path: u8*, new_ext: u8*) {
    let idx = find_extension_start(path);
    strcpy(path + idx, new_ext);
}

fn is_space(c: u8): bool {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

fn is_digit(c: u8): bool {
    return c >= '0' && c <= '9';
}

fn is_alpha(c: u8): bool {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

fn is_alnum(c: u8): bool {
    return is_digit(c) || is_alpha(c);
}

fn is_hex(c: u8): bool {
    return is_digit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

fn is_octal(c: u8): bool {
    return c >= '0' && c <= '7';
}

///////////////////////////////////////////////////////////////////////////////
// I/O

const stdin = 0;
const stdout = 1;
const stderr = 2;

fn putc(c: u8) {
    write(0, &c, 1);
}

fn puts(c: u8 *) {
    let len: u64 = strlen(c);
    write(1, c, len);
}

fn putsln(c: u8 *) {
    puts(c);
    putc('\n');
}

fn putu_buffer(n: u64, buf: u8*): u64 {
    let i: u64 = 0;
    while (n > 0) {
        buf[i] = (n % 10) + '0';
        n = n / 10;
        i = i + 1;
    }
    if (i == 0) {
        buf[i] = '0';
        i = i + 1;
    }
    buf[i] = 0;
    if (i > 1)
        strrev(buf);
    return i;
}

fn puti_buffer(n: i64, buf: u8*): u64 {
    let i: u64 = 0;
    let sign: i64 = 1;
    if (n < 0) {
        sign = -1;
        n = -n;
    }
    while (n > 0) {
        buf[i] = (n % 10) + '0';
        n = n / 10;
        i = i + 1;
    }
    if (i == 0) {
        buf[i] = '0';
        i = i + 1;
    }
    if (sign < 0) {
        buf[i] = '-';
        i = i + 1;
    }
    buf[i] = 0;
    if (i > 1)
        strrev(buf);
    return i;
}

fn putu(n: u64) {
    let buf: u8[32];
    let len = putu_buffer(n, buf);
    write(stdout, buf, len);
}

fn puti(n: i64) {
    let buf: u8[32];
    let len = puti_buffer(n, buf);
    write(stdout, buf, len);
}

fn putflt(f: f64) {
    let before: i64 = f;
    let after: f64 = f - before;
    if (before < 0) {
        putc('-');
        before = -before;
        after = -after;
    }
    putu(before);
    putc('.');
    for (let i = 0; i < 5; ++i) {
        let d: u64 = after * 10;
        putc(d + '0');
        after = after * 10 - d;
    }
}

// You want to call these functions with `here` as the first parameter.
fn die(sloc: u8 *, msg1: u8 *) {
    puts(sloc);
    puts(": ");
    putsln(msg1);
    exit(1);
}

fn die2(sloc: u8 *, msg1: u8 *, msg2: u8*) {
    puts(sloc);
    puts(": ");
    puts(msg1);
    putsln(msg2);
    exit(1);
}

// TODO: file/line numbers would be helpful
fn assert(sloc: u8*, cond: bool) {
    if (!cond)
        die(sloc, "assertion failed");
}

///////////////////////////////////////////////////////////////////////////////
// Math

fn min(a: u64, b: u64): u64 {
    return a < b ? a : b;
}

fn max(a: u64, b: u64): u64 {
    return a > b ? a : b;
}

fn sign(a: u64): u64 {
    return a > 0 ? 1 : a == 0 ? 0 : -1;
}

fn abs(a: u64): u64 {
    return a * sign(a);
}

fn factorial(n: u64): u64 {
    let res: u64 = 1;
    for (;n > 0; n = n - 1)
        res = res * n;
    return res;
}

fn align_up(val: u64, algn: u64): u64 {
    return (val + algn - 1) & ~(algn - 1);
}

///////////////////////////////////////////////////////////////////////////////
// External processes

fn WIFEXITED(status: u64): bool {
    return (status & 127) == 0;
}

fn WEXITSTATUS(status: u64): u64 {
    return (status >> 8) & 127;
}

fn run_command_env(args: u8**, envp: u8**, echo: u64) {
    if (echo) {
        puts("[+]");
        for (let c = args; *c; ++c) {
            puts(" "); puts(*c);
        }
        puts("\n");
    }

    let pid = fork();
    if (pid == 0) {
        execve(args[0], args, envp);
        die(here, "Error in execve()");
    }
    // Parent
    let status: u64;
    if (wait(&status) < 0)
        die(here, "Error in wait()");

    if (!WIFEXITED(status))
        die(here, "Child did not exit normally");

    let exit_status = WEXITSTATUS(status);
    if (exit_status != 0) {
        puts(here); puts(": Child exited with non-zero status: (");
        putu(exit_status);
        putsln(")");
        exit(exit_status);
    }
}

fn run_command(args: u8**, echo: u64) {
    run_command_env(args, null, echo);
}