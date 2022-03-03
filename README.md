## TEECap C compiler

This is a simple compiler that translates a subset of C into 
TEECap in a format that can be run on the TEECap emulator.

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

The standard output is in a format that can be directly run in the
TEECap emulator. Therefore, you can pipe them for better convenience.

You can start with the C files in the `samples` folder.

Two environment variables influence the compiler behaviours:
* `TEECAP_SHOW_AST`: if it is set, the compiler will also display the syntax tree of the source file
display the syntax tree of the source file
* `TEECAP_PRINT_COMMENTS`: if it is set, the compiler will emit in the
  output.

