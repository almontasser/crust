import "std/memory.cr"

struct Vector {
    size: u64;
    capacity: u64;
    data: void**;

    fn new() {
        self.size = 0;
        self.capacity = 8;
        self.data = malloc(self.capacity * sizeof(void*));
    }

    fn empty(): bool {
        return self.size == 0;
    }

    fn at(index: u64): void* {
        if (index >= self.size) die(here, "Index out of bounds");
        return self.data[index];
    }

    fn push(item: void*) {
        if (self.size == self.capacity) {
            let new_capacity = self.capacity * 2;
            let new_data = malloc(new_capacity * sizeof(void*));
            memcpy(new_data, self.data, self.size * sizeof(void*));
            self.data = new_data;
            self.capacity = new_capacity;
        }
        self.data[self.size] = item;
        self.size += 1;
    }

    fn pop(): void* {
        if (self.size == 0) die(here, "Vector is empty");
        self.size -= 1;
        return self.data[self.size];
    }

    fn top(): void* {
        if (self.size == 0) die(here, "Vector is empty");
        return self.data[self.size - 1];
    }

    fn delete() {
        free(self.data);
    }
}