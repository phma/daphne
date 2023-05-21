module Main (main) where

import Cryptography.Daphne
import Text.Printf
import Data.Array

main :: IO ()
main = putStrLn $ printf "%02x" (sbox ! 0x11)
