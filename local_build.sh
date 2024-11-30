#!/bin/bash

check_rust_toolchain() {
    if ! command -v rustc &> /dev/null; then
        echo "Rust is not installed. Installing Rust..."
        
        if command -v curl &> /dev/null; then
            curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
            source "$HOME/.cargo/env"
        else
            echo "Error: curl is not installed. Cannot download Rust."
            exit 1
        fi
    fi

    # rustc --version
    # cargo --version
}


main() {
     check_rust_toolchain
     cargo clean
     cargo build --release
    echo "CAPSTONE-C compiled successfully!"
}

main "$@"