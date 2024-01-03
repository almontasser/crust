use std::time::{SystemTime, UNIX_EPOCH};

pub struct RandomGenerator {
    x: u64,
    y: u64,
    w: u64,
    z: u64,
}

impl RandomGenerator {
    pub fn new() -> RandomGenerator {
        // get current time
        let now = SystemTime::now();
        let seed = now.duration_since(UNIX_EPOCH).unwrap().as_secs();
        RandomGenerator {
            x: 123456789 ^ seed,
            y: 362436069 ^ seed,
            w: 886754123 ^ seed,
            z: 521288629 ^ seed,
        }
    }

    pub fn random(&mut self) -> u64 {
        let t = self.x ^ (self.x << 11);
        self.x = self.y;
        self.y = self.w;
        self.w = self.z;
        self.z = self.z ^ (self.z >> 19) ^ (t ^ (t >> 8));

        self.z
    }
}
