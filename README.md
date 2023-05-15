# Overview
Daphne is a self-synchronizing byte stream cipher. The code is written in both Rust and Haskell.

# Rust
To run the program, type `cargo run`. The first time you run it, Cargo may download a quarter-gigabyte of data, which is the index to all crates.

# Haskell
You can run Daphne in the REPL with `stack ghci` or use other Stack commands.

# Testing both
The test to make sure that both implementations do the same thing is not written yet.
