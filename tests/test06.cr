import "std/memory.cr"

struct Point {
    x: u32;
    y: u32;

    fn new(x: u32, y: u32) {
        self.x = x;
        self.y = y;
    }

    fn print() {
        puts("Point(");
        putu(self.x);
        puts(", ");
        putu(self.y);
        putsln(")");
    }
}

fn main() {
    for (let i = 0; i < 10; ++i) {
        let p: Point* = new Point(i, i * 2);
        p.print();
        continue;
    }
    let i = 0;
    while (i < 10) {
        let p: Point* = new Point(i, i * 2);
        p.print();
        ++i;
        continue;
    }
    for (i = 0; i < 10; ++i) {
        break;
        let p: Point* = new Point(i, i * 2);
        p.print();
        continue;
    }
    i = 0;
    while (i < 10) {
        break;
        let p: Point* = new Point(i, i * 2);
        p.print();
        ++i;
        continue;
    }
}
