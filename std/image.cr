import "std/common.cr"
import "std/file.cr"
import "std/memory.cr"

struct Color8 {
    r: u8;
    g: u8;
    b: u8;
}

struct Image {
    width: u32;
    height: u32;
    data: Color8*;

    fn set(x: i32, y: i32, color: Color8*) {
        let idx = y * self.width + x;
        self.data[idx].r = color.r;
        self.data[idx].g = color.g;
        self.data[idx].b = color.b;
    }

    fn get(x: i32, y: i32, color: Color8*) {
        let idx = y * self.width + x;
        color.r = self.data[idx].r;
        color.g = self.data[idx].g;
        color.b = self.data[idx].b;
   }

    fn save(filename: u8*) {
        let file = fopen(filename, 'w');
        defer file.close();

        file.puts("P6\n");
        file.putu(self.width);
        file.putc(' ');
        file.putu(self.height);
        file.putc('\n');
        file.putu(255);
        file.putc('\n');
        file.write(self.data, self.width * self.height * sizeof(Color8));
    }
}

fn new_image(width: u32, height: u32): Image* {
    let img: Image* = malloc(sizeof(Image));
    img.width = width;
    img.height = height;
    img.data = malloc(width * height * sizeof(Color8));
    return img;
}

fn read_image(filename: u8*): Image* {
    let buf: u8[64];

    let file = fopen(filename, 'r');
    defer file.close();

    let n = file.read(buf, 64);
    if (n < 3 || buf[0] != 'P' || buf[1] != '6')
        die2(here, "Not a valid PPM file: ", filename);

    // FIXME: This is very hacky.
    let cur = buf + 3;                        // Skip "P6\n"
    let width = atoi_end(cur, &cur); ++cur;
    let height = atoi_end(cur, &cur); ++cur;
    let max_val = atoi_end(cur, &cur); ++cur;
    if (max_val != 255)
        die2(here, "Only PPM files with max value 255 are supported:", filename);

    // Seek to start of binary data
    file.seek(cur - buf, SEEK_SET);

    let img = new_image(width, height);
    n = file.read(img.data, width * height * sizeof(Color8));
    if (n != width * height * sizeof(Color8))
        die2(here, "Could not read image data:", filename);
    return img;
}
