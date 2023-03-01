## Capstone C compiler

This is a simple compiler that translates a subset of C into 
Capstone in a format that can be run on the Capstone emulator.

### Setting up

To get started, set up the Rust development toolchain as per
the instructions here: https://rustup.rs/

The nightly channel is needed for this project,
so after installing rustup, install the nightly toolchain:
    
    rustup toolchain install nightly

and switch to the nightly toolchain (run this in the project folder):

    rustup override set nightly

To build the project, run the following in the project folder:
cargo build

### Usage

In the project folder, use the following command to run the compiler:

    cargo run <path-to-source-file>

The compiler takes some options which can either be used to set a custom machine start state, or to help with debugging the output (`--help` or `-h`).

    OPTIONS:
        --clock-rate <CLOCK_RATE>    [default: 500]
        --gpr-n <GPR_N>              [default: 32]
    -h, --help                       Print help information
        --mem-size <MEM_SIZE>        [default: 65536]
        --print-comments             
        --show-ast                   
        --stack-size <STACK_SIZE>    [default: 16384]
    -V, --version                    Print version information


The standard output is in a format that can be directly run in the
Capstone emulator. Therefore, you can pipe them for better convenience.

You can start with the C files in the `samples` folder.


