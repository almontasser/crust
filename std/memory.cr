import "std/common.cr"

// FIXME: This is horrible for performance.
fn memcpy(dest: void*, src: void*, size: u64): void* {
    let d: u8* = dest;
    let s: u8* = src;
    for (let i: u32 = 0; i < size; i = i + 1) {
        d[i] = s[i];
    }
    return dest;
}

const __MALLOC_BUF_SIZE = 1024 * 1024 * 1024; // 1 GB
let __FREE_CLEAR_RESET_MEM = true;
let __malloc_buf: u8[__MALLOC_BUF_SIZE];
let __malloc_buf_pos = 0;
let __malloc_init = false;

struct __MEM_SEGMENT{
    mem: u32;
    ptr: u32; // because of weird stuff with pointer calculations I used an array index
    next: __MEM_SEGMENT*;
    istaken: bool;
}

// Initializes initial __MEM_SEGMENT
fn __malloc_init_proc(){
    if (!__malloc_init){
        let in_: void* =  &__malloc_buf[0];
        let initial_segment: __MEM_SEGMENT* = in_;
        initial_segment.mem = __MALLOC_BUF_SIZE - sizeof(__MEM_SEGMENT);
        initial_segment.ptr = sizeof(__MEM_SEGMENT);
        initial_segment.next = null;
        initial_segment.istaken = false;
        __malloc_init = true;
    }
}

fn transmute(ptr: void*): void*{
    return ptr;
}

fn malloc(size: u64): void*{
    // get first __MEM_SEGMENT
    __malloc_init_proc();
    let in_: void* =  &__malloc_buf[0];
    let segment: __MEM_SEGMENT* = in_;
    while (true){
        if (!segment.istaken){
            if (((segment.mem - size) - sizeof(__MEM_SEGMENT)) > 0){
                // divides a segment into one with requested size and one with remaining memory

                let after = segment.next;
                let segment_mem = segment.mem;
                segment.mem = size;
                segment.next = transmute(&__malloc_buf[segment.ptr + size]);
                segment.istaken = true;

                let left_memory: __MEM_SEGMENT* = segment.next;
                left_memory.ptr = segment.ptr + size + sizeof(__MEM_SEGMENT);
                left_memory.istaken = false;
                left_memory.mem = segment_mem - size;
                left_memory.mem = left_memory.mem - sizeof(__MEM_SEGMENT);
                left_memory.next = after;

                return transmute(&__malloc_buf[segment.ptr]);
            } else{
                if (segment.next == null){
                    die(here, "malloc: out of memory. only 1gb available");
                    return null;
                }
                segment = segment.next;
            }
        } else{
            if (segment.next == null){
                die(here, "malloc: out of memory. only 1gb available");
                return null;
            }

            segment = segment.next;
        }
    }
}

fn __free_clear_ptr(ptr: u64, mem: u64){
    for (let i: u64 = 0; i < mem; ++i){
        __malloc_buf[ptr+i] = 0;
    }
}

fn __find_segment_by_ptr(ptr: void*): __MEM_SEGMENT*{
    let in_: void* =  &__malloc_buf[0];
    let prv_segment: __MEM_SEGMENT* = null;
    let segment: __MEM_SEGMENT* = in_;
    while (true){
        if (&__malloc_buf[segment.ptr] == ptr){
            return segment;
        } else{

            if (segment.next == null){
                die(here, "didn't find malloc cell");
                return null;
            }

            prv_segment = segment;
            segment = segment.next;
        }
    }

    return null;
}

fn free(ptr: void*) {
    __malloc_init_proc();
    let segment: __MEM_SEGMENT* = __find_segment_by_ptr(ptr);

    // this behaviour can be disabled
    if (__FREE_CLEAR_RESET_MEM){
        __free_clear_ptr(segment.ptr, segment.mem);
    }

    segment.istaken = false;
}

fn realloc(ptr: void*, size: u64): void*{
    if (size < 0){
        die(here, "realloc: size can't be a negative int");
        return null;
    } else if (size == 0) {
        die(here, "realloc: size can't be zero; use free instead");
        return null;
    }

    let segment: __MEM_SEGMENT* = __find_segment_by_ptr(ptr);

    if (segment.mem >= size){
        die(here, "realloc: you can't realloc a memory cell into a cell with a smaller size");
        return null;
    }

    let new_mem: void* = malloc(size);
    memcpy(new_mem, ptr, segment.mem);
    free(ptr);
    return new_mem;
}

fn __dbg_malloc(){
    __malloc_init_proc();

    let in_: void* =  &__malloc_buf[__malloc_buf_pos];
    let segment: __MEM_SEGMENT* = in_;

    putsln("DEBUG MALLOC");
    while (segment != null){
        putsln("=== SEGMENT: ");
        putsln("PTR: ");
        print(segment.ptr);
        putsln("MEM: ");
        print(segment.mem);
        putsln("NEXT: ");
        print(segment.next);
        if (segment.istaken){
            putsln("TAKEN");
        }
        putsln("===");
        segment = segment.next;
    }
}