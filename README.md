## Capstone C compiler

This is an experimental compiler that compiles a C subset (with Capstone-specific
extensions) to Capstone-RISC-V.

### Setting up

To get started, set up the Rust development toolchain as per
the instructions here: https://rustup.rs/

To build the project, run the following in the project folder:

    cargo build

### Usage

In the project folder, use the following command to run the compiler:

    cargo run -- <path-to-source-file>

The compiler invokes gcc's C preprocessor. You can pass arguments through
to gcc, e.g., to specify the include paths, by appending them after `--`:

    cargo run -- sample.c -- -I../include

The default target is for the base RV64 ISA and uses an ABI compatible with
the standard one. It is hence possible to link the result with code compiled
by gcc.

To switch to the Capstone target, supply `--abi capstone`:

    cargo run -- --abi capstone sample.c


The standard output is assembly code that can be fed to the GNU toolchain
to get the ELF object file:

    riscv64-unknown-linux-gnu-gcc -c -o output.o output.S

