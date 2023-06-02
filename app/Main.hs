module Main (main) where

import Cryptography.Daphne
import Text.Printf
import Data.Array
import Data.Word
import Data.List.Split

lineStr :: [Word8] -> String
lineStr [] = ""
lineStr (a:as) = (printf "%02x " a)++(lineStr as)

blockStr :: [[Word8]] -> String
blockStr [] = ""
blockStr (a:as) = (lineStr a) ++ "\n" ++ (blockStr as)

block16str :: [Word8] -> String
-- Takes a list of 256 bytes and formats them 16 to a line.
block16str a = blockStr $ chunksOf 16 a

stepFixedPoints :: Word8 -> Word8 -> [Word8]
stepFixedPoints l r = [ x | x <- [0..255], step x l r == x ]

main :: IO ()
main = putStr $ block16str $ map (\x -> step x 243 125) [0..255]
