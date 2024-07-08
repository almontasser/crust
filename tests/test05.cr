struct Test {
    a: i32,
    b: i32,

    fn print() {
        print(self.a);
        print(self.b);
    }

    fn add(other: Test*) {
        self.a = self.a + other.a;
        self.b = self.b + other.b;
    }
}

union U {
    a: u32,
    b: u8,
}

fn main() {
    let t: Test;
    let t2: Test;
    t.a = 10;
    t.b = 20;
    t2.a = 30;
    t2.b = 40;
    print(t.a);
    print(t.b);
    print(0);
    t.print();
    print(0);
    t.add(&t2);
    t.print();
    let u: U;
    u.a = 0x12345678;
    print(u.b);
    return;
}