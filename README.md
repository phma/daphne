# Overview
![](https://github.com/phma/daphne/blob/master/img/diagram.png?raw=true)
Daphne is a self-synchronizing byte stream cipher. Plaintext bytes are added to the accumulator, which runs the gantlope between the key (shown as 16 bytes, but it can be longer) and a shift register of as many previous ciphertext bytes, resulting in two keystream bytes, which encrypt the plaintext bytes using two different multiplications and an S-box. The same two multiplications and S-box are used in the gantlope to turn the accumulator into the keystream.

Decryption is similar; it uses the corresponding divisions and the inverse S-box. If the decrypter is out of sync with the encrypter, it will resynchronize when the shift register is fed enough bytes and the accumulators match.

The code is written in both Rust and Haskell.

# Rust
To run the program, type `cargo run`. The first time you run it, Cargo may download a quarter-gigabyte of data, which is the index to all crates.

# Haskell
You can run Daphne in the REPL with `stack ghci` or use other Stack commands.

# Testing both
Run ``./test.sh` . This runs both implementations, check that the outputs are identical, and check that a certain sequence of bytes is present.
