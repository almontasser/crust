import "std/memory.cr"

struct Point {
    x: u32;
    y: u32;

    fn new(x: u32, y: u32) {
        self.x = x;
        self.y = y;
    }

    fn delete() {
        puts("Point(");
        putu(self.x);
        puts(", ");
        putu(self.y);
        putsln(") deleted");
    }
}

fn main() {
    let p: Point* = new Point(10, 20);
    print(p);
    delete p();
    print(p);
}