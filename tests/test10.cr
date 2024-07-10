import "std/file.cr"
import "std/random.cr"
import "std/vector.cr"

fn main() {
    let p: void* = -1;
    print(p);
    print(random.rand());
    print(random.rand());
    print(random.rand());
    let f: File* = fopen("/home/mahmoud/projects/crust20/tests/test11.cr", 'a');
    f.write("import \"std/memory.cr\"\n\n", 24);
    delete f;

    let v = new Vector();
    for (let i = 0; i < 10; i += 1) {
        v.push(i);
    }
    while (!v.empty()) {
        print(v.pop());
    }
    delete v;
}