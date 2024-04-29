![Crust](https://github.com/almontasser/crust/assets/19656179/6ac3df79-264b-464b-a180-11d1870445c8)

# CRUST

This is a hobby project to learn about compilers and language design. I've designed the language to be similar to C and Rust.

Heavily inspired by https://github.com/DoctorWkt/acwj

## Features

- [x] Global Variables
- [x] Functions
- [x] Arrays
- [x] Integers (signed and unsigned)
- [x] Strings
- [x] Binary Operations
- [x] Code Generation (GNU Assembly)
- [x] Print to Console (integers & ascii characters)
- [x] If Statements
- [x] While Statements
- [x] For Statements
- [x] Function Parameters
- [ ] Local Variables (Scopes)
- [ ] Reading from console
- [ ] Dynamic Arrays
- [ ] Structs
- [ ] Unions
- [ ] Enums
- [ ] Break & Continue
- [ ] Variable Initialization
- [ ] Casting
- [ ] Sizeof
- [ ] Static
- [ ] Struct Methods
- [ ] Struct Traits
- [ ] LLVM

## How to use

```sh
cargo run <input-file> # Compile the crust language to assembly, which will be written to out.s
cc -o bin out.s # Use the GNU C compiler to compile and link the assembly code to an executable file
./bin # Execute the produced binary
```

## Run tests

```sh
./runtests.sh
```

## Examples

Some examples of the language

```rust
let c: char;
let str: *char;

fn main(): u8 {
  c= '\n'; printint(c);

  for (str= "Hello world\n"; *str != 0; str= str + 1) {
    printchar(*str);
  }
  return 0;
}
```

```rust
let a: u8;
let b: u8[25];

fn main(): u32 {
  b[3]= 12; a= b[3];
  printint(a);
  return 0;
}
```
