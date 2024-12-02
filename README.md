## 🛡️ Capstone C compiler

This is an experimental compiler that compiles a C subset (with Capstone-specific
extensions) to Capstone-RISC-V.

### Setting up

> To build the compiler on local machine use the build script:

```bash
    ./local_build.sh
```
> To build the docker container (Setup docker on your machine first):

```bash
    docker build -t capstone-c
```
> To interact with the container

```bash 
    docker run -it capstone-c bash 
```

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

