let seeded = false;

struct RandomState {
    x: i32;
    y: i32;
    z: i32;
    w: i32;

    fn rand(): i32 {
        if (!seeded) {
            self.x = 123456789;
            self.y = 362436069;
            self.z = 521288629;
            self.w = 88675123;
            seeded = true;
        }

        let t = self.x ^ (self.x << 11);
        self.x = self.y;
        self.y = self.z;
        self.z = self.w;
        let ret = self.w = (self.w ^ (self.w >> 19)) ^ (t ^ (t >> 8));
        return ret;
    }

    fn rand01(): f64 {
        return self.rand() / (1 << 31 + 0.0);
    }
}

let random: RandomState;

