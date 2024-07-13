import "std/common.cr"
import "std/memory.cr"

const __FILE_BUFFER_CAP = 1024;
const O_RDONLY = 00;
const O_WRONLY = 01;
const O_RDWR = 02;
const O_CREAT = 0100;
const O_TRUNC = 01000;
const O_APPEND = 02000;

const SEEK_SET = 0;
const SEEK_CUR = 1;
const SEEK_END = 2;

struct File {
    fd: i64;
    name: u8*;
    // Buffer
    buffer: u8[__FILE_BUFFER_CAP];
    buffer_size: u64;

    fn flush() {
        if (self.buffer_size > 0) {
            let written = write(self.fd, self.buffer, self.buffer_size);
            if (written < 0) die2(here, "Failed to write to file: ", self.name);
            self.buffer_size = 0;
        }
    }

    fn write(buf: void*, size: u64) {
        if (self.buffer_size + size > __FILE_BUFFER_CAP) {
            self.flush();
        }
        if (size > __FILE_BUFFER_CAP) {
            let written = write(self.fd, buf, size);
            if (written < 0) die2(here, "Failed to write to file", self.name);
        } else {
            memcpy(self.buffer + self.buffer_size, buf, size);
            self.buffer_size += size;
        }
    }

    fn read(buf: void*, size: u64): u64 {
        let n = read(self.fd, buf, size);
        if (n < 0) die2(here, "Failed to read from file: ", self.name);
        return n;
    }

    fn close() {
        self.flush();
        close(self.fd);
        self.fd = -1;
    }

    fn puts(s: u8*) {
        self.write(s, strlen(s));
    }

    fn putc(c: u8) {
        self.write(&c, 1);
    }

    fn putu(u: u64) {
        let buf: u8[32];
        let len = putu_buffer(u, buf);
        self.write(buf, len);
    }

    fn seek(offset: i64, whence: u8) {
        let n = lseek(self.fd, offset, whence);
        if (n < 0)
            die2(here, "Failed to seek in file: ", self.name);
    }

    fn size(): u64 {
        let current = lseek(self.fd, 0, SEEK_CUR);
        if (current < 0)
            die2(here, "Failed to get file size: ", self.name);
        let size = lseek(self.fd, 0, SEEK_END);
        if (size < 0)
            die2(here, "Failed to get file size: ", self.name);
        lseek(self.fd, current, SEEK_SET);
        return size;
    }

    fn map(sizeptr: u64*): u8* {
        let size = self.size();
        let buf: u8* = mmap(null, size, PROT_READ | PROT_WRITE, MAP_PRIVATE, self.fd, 0);
        if (buf == MAP_FAILED)
            die2(here, "Failed to map file: ", self.name);
        if (sizeptr)
            *sizeptr = size;
        return buf;
    }

    fn slurp(sizeptr: u64*): u8* {
        let size = self.size();
        let buf: u8* = malloc(size+1);
        self.read(buf, size);
        buf[size] = 0;
        if (sizeptr) *sizeptr = size;
        return buf;
    }

    fn delete() {
        self.close();
    }
}

fn fopen(name: u8*, mode: u8): File* {
    let open_mode: u64;
    if (mode == 'w') open_mode = O_WRONLY | O_CREAT | O_TRUNC;
    else if (mode == 'r') open_mode = O_RDONLY;
    else if (mode == 'a') open_mode = O_WRONLY | O_CREAT | O_APPEND;
    else die(here, "Unknown file open mode");

    let f: File* = malloc(sizeof(File));
    f.name = name;
    f.fd = open(name, open_mode, 0666);
    if (f.fd < 0) die2(here, "Failed to open file: ", name);
    f.buffer_size = 0;
    return f;
}