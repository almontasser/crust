import "../std/memory.cr"

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
    let p: Point* = new Point(10, 20);
    p.print();
}
