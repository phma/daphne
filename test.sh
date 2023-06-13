#!/bin/sh
#This script runs the Rust and Haskell implementations of Daphne and compares
#their outputs. They should be identical.

cargo run >/tmp/rust.out
stack run >/tmp/haskell.out

if diff -u /tmp/rust.out /tmp/haskell.out && grep "28 75 b5" /tmp/rust.out
then echo "Test passed"
fi
