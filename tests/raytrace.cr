//
// Code adapted from Ray Tracing in One Weekend by Peter Shirley
//              http://raytracing.github.io/
//

import "std/vector.cr"
import "std/image.cr"
import "std/random.cr"

let TOL = 0.00001;

struct Vec {
    x: f64;
    y: f64;
    z: f64;

    fn init(x: f64, y: f64, z: f64) {
           self.x = x;
           self.y = y;
           self.z = z;
       }

    fn add(other: Vec*) {
           self.x = self.x + other.x;
           self.y = self.y + other.y;
           self.z = self.z + other.z;
       }

    fn sub(other: Vec*) {
        self.x = self.x - other.x;
        self.y = self.y - other.y;
        self.z = self.z - other.z;
    }

    fn dot(other: Vec*): f64 {
           return self.x * other.x + self.y * other.y + self.z * other.z;
       }

    fn length(): f64 {
           return sqrt(self.x * self.x + self.y * self.y + self.z * self.z);
       }

    fn length_sq(): f64 {
           return self.x * self.x + self.y * self.y + self.z * self.z;
       }

    fn normalize() {
           let len = self.length();
           self.x = self.x / len;
           self.y = self.y / len;
           self.z = self.z / len;
       }

    fn copy_from(other: Vec*) {
        self.x = other.x;
        self.y = other.y;
        self.z = other.z;
    }

    fn scale(other: f64) {
           self.x = self.x * other;
           self.y = self.y * other;
           self.z = self.z * other;
       }

    fn mult(other: Vec*) {
           self.x = self.x * other.x;
           self.y = self.y * other.y;
           self.z = self.z * other.z;
       }
}


// Methods to help generate random vectors

fn random_vec(vec: Vec*) {
    vec.x = random.rand01() * 2.0 - 1.0;
    vec.y = random.rand01() * 2.0 - 1.0;
    vec.z = random.rand01() * 2.0 - 1.0;
}

fn random_vec_unit(vec: Vec*) {
    while (true) {
        random_vec(vec);
        if (vec.length_sq() < 1) {
            vec.normalize();
            return;
        }
    }
}

struct Ray {
    ori: Vec;
    dir: Vec;

    fn at(t: f64, res: Vec*) {
           res.x = self.ori.x + self.dir.x * t;
           res.y = self.ori.y + self.dir.y * t;
           res.z = self.ori.z + self.dir.z * t;
       }
}

struct Sphere {
    center: Vec;
    radius: f64;
    color: Vec;

    fn hit(ray: Ray*, t: f64*, n: Vec*, col: Vec*): bool {
           let oc: Vec;
           oc.init(ray.ori.x - self.center.x,
                    ray.ori.y - self.center.y,
                    ray.ori.z - self.center.z);
           let a = ray.dir.dot(&ray.dir);
           let b = 2.0 * oc.dot(&ray.dir);
           let c = oc.dot(&oc) - self.radius * self.radius;
           let disc = b * b - 4.0 * a * c;
           if (disc < 0.0)
               return false;
           let t0 = (-b - sqrt(disc)) / (2.0 * a);
           let t1 = (-b + sqrt(disc)) / (2.0 * a);

           let best = t0;
           if (best < TOL) best = t1;
           if (best < TOL) return false;

           *t = best;
           col.copy_from(&self.color);
           ray.at(best, n);
           n.sub(&self.center);
           n.normalize();
           return true;
       }
}

fn new_sphere(x: f64, y: f64, z: f64, radius: f64, r: f64, g: f64, b: f64): Sphere* {
    let s: Sphere* = malloc(sizeof(Sphere));
    s.center.init(x, y, z);
    s.color.init(r, g, b);
    s.radius = radius;
    return s;
}

fn background_color(ray: Ray*, color: Vec*) {
    let t = 0.5 * (ray.dir.y + 1.0);
    let col2: Vec;
    col2.init(1.0, 1.0, 1.0);
    col2.scale(1-t);

    color.init(0.5, 0.7, 1.0);
    color.scale(t);
    color.add(&col2);
}

fn find_hit(ray: Ray*, objs: Vector*, t: f64*, n: Vec*, obj_col: Vec*): i64 {
    let idx = -1;

    for (let i = 0; i < objs.size; ++i) {
        let obj: Sphere* = objs.at(i);

        let tmp_t: f64;
        let tmp_n: Vec;
        let tmp_col: Vec;

        if (obj.hit(ray, &tmp_t, &tmp_n, &tmp_col)) {
            if (*t < 0 || tmp_t < *t) {
                *t = tmp_t;
                idx = i;
                obj_col.copy_from(&tmp_col);
                n.copy_from(&tmp_n);
            }
        }
    }
    return idx;
}

fn raytrace(ray: Ray*, objs: Vector*, color: Vec*, depth: i64) {
    if (depth < 0)
        return color.init(0, 0, 0);

    let t: f64 = -1;
    let n: Vec;
    let obj_col: Vec;

    let hit_idx = find_hit(ray, objs, &t, &n, &obj_col);
    if (hit_idx < 0)
        return background_color(ray, color);

    let rec_col: Vec;
    ray.at(t, &ray.ori);

    let target: Vec;
    random_vec_unit(&target);
    target.add(&n);
    ray.dir.copy_from(&target);

    raytrace(ray, objs, &rec_col, depth - 1);
    rec_col.mult(&obj_col);
    color.copy_from(&rec_col);
}

fn to_color8(fcol: Vec*, ucol: Color8*) {
    ucol.r = min(255, max(0, (fcol.x * 255.0)));
    ucol.g = min(255, max(0, (fcol.y * 255.0)));
    ucol.b = min(255, max(0, (fcol.z * 255.0)));
}

fn main() {
    let objs = new Vector();
    objs.push(new_sphere(0, 0, -1, 0.5, 1, 0.6, 0.3));
    objs.push(new_sphere(0, -100.5, -1, 100, 0.5, 0.5, 0.5));

    // Image
    let aspect_ratio = 16.0 / 9.0;
    let image_width: i32 = 800;
    let image_height: i32 = image_width / aspect_ratio;
    let samples_per_pixel: i32 = 25;

    // Camera
    let viewport_height = 2.0;
    let viewport_width = aspect_ratio * viewport_height;
    let focal_length = 1.0;

    let origin: Vec; origin.init(0, 0, 0);
    let horizontal: Vec; horizontal.init(viewport_width, 0, 0);
    let vertical: Vec; vertical.init(0, viewport_height, 0);

    let ll_corner: Vec;
    ll_corner.x = origin.x - horizontal.x/2 - vertical.x/2 - 0;
    ll_corner.y = origin.y - horizontal.y/2 - vertical.y/2 - 0;
    ll_corner.z = origin.z - horizontal.z/2 - vertical.z/2 - focal_length;

    let img = new_image(image_width, image_height);

    for (let y = 0; y < image_height; ++y) {
        putc(13); putu(y); putc('/'); putu(image_height); puts(" done");
        for (let x = 0; x < image_width; ++x) {
            let total_col: Vec;
            total_col.init(0, 0, 0);
            for (let s = 0; s < samples_per_pixel; ++s) {
                let u = (random.rand01() + x) / (image_width-1);
                let v = 1 - (random.rand01() + y) / (image_height-1);
                let ray: Ray;
                ray.ori.copy_from(&origin);
                ray.dir.init(
                    ll_corner.x + u * horizontal.x + v * vertical.x - origin.x,
                    ll_corner.y + u * horizontal.y + v * vertical.y - origin.y,
                    ll_corner.z + u * horizontal.z + v * vertical.z - origin.z
                );
                ray.dir.normalize();

                let color: Vec;
                raytrace(&ray, objs, &color, 5);
                total_col.add(&color);
            }

            total_col.scale(1.0 / samples_per_pixel);
            let color_out: Color8;
            to_color8(&total_col, &color_out);
            img.set(x, y, &color_out);
        }
    }
    putc('\n');
    img.save("out.ppm");
}