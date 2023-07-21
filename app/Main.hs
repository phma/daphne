module Main (main) where

import Cryptography.Daphne.Internals
import Cryptography.Daphne
import Text.Printf
import Data.Array
import Data.Word
import Data.List.Split
import Stats
import Cryptanalysis
import System.Environment

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

zerodaph = keyDaphne (replicate 16 0)
squaredaph = keyDaphne (take 16 (map (^2) [0..]))
sqcrypt = snd $ listEncrypt squaredaph [0..255]

doArg :: String -> IO Bool
doArg x 
  | x == "chosen-cipher" = do
    chosenCiphertext
    return True
  | x == "chosen-plain" = do
    chosenPlaintext
    return True
  | otherwise = return False

doArgs :: [String] -> IO Bool
doArgs [] = return True
doArgs (x:xs) = do
  validx <- doArg x
  validxs <- doArgs xs
  return (validx && validxs)

usage :: IO ()
usage = do
  putStrLn "usage"

main :: IO ()
main = do
  args <- getArgs
  if null args
     then do
	putStrLn "Encrypt all 0s with all 0s"
	putStr $ block16str $ snd $ listEncrypt zerodaph (replicate 256 0)
	putStrLn "Encrypt 0..255 with squares"
	putStr $ block16str $ sqcrypt
	putStrLn "Decrypt the above"
	putStr $ block16str $ snd $ listDecrypt squaredaph sqcrypt
	putStrLn "Demonstrate resynchronization"
	putStr $ block16str $ snd $ listDecrypt squaredaph (2:(tail sqcrypt))
     else do
     valid <- doArgs args
     if valid
	then return ()
	else usage

