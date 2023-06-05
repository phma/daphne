module Cryptography.Daphne
  ( twist
  , sbox
  , invSbox
  , mulOdd
  , invOdd
  , divOdd
  , mul257
  , inv257
  , div257
  , step
  , invStep
  , Daphne
  , emptyDaphne
  , keyDaphne
  ) where

import Data.Bits
import Data.Array.Unboxed
import Data.Word
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))

-- | If a has at least 3 bits and k is relatively prime to the number of bits
-- in a, this permutation satisfies the strict avalanche criterion.
twist :: Bits a => Int -> a -> a
twist k n = rotate n ((popCount n)*k)

shuffle :: Word8 -> Word8
shuffle n = (rotate (n .&. 0x54) 3) .|. (rotate (n .&. 0x28) 7) .|.
            (rotate (n .&. 0x02) 5) .|. (rotate (n .&. 0x80) 4) .|. (n .&. 1)

funSbox :: Word8 -> Word8
funSbox = (xor 0x6e) . shuffle . (twist (-1)) . (xor 0x25)

sbox = array (0,255)
  [ (i,funSbox i) | i <- [0..255] ]
  :: Array Word8 Word8

invSbox = array (0,255)
  [ (funSbox i,i) | i <- [0..255] ]
  :: Array Word8 Word8

-- Equivalent to shifting m and n left by 1 with a 1 bit (thus making them odd),
-- multiplying, and shifting right (discarding the 1 bit).
mulOdd :: Integral a => a -> a -> a
mulOdd m n = m + n + 2 * m * n

-- It's a byte operation, but involves a two-byte multiplication.
mul257' :: Word -> Word -> Word
mul257' a 0 = 257 - a
mul257' 0 b = 257 - b
mul257' a b = (a * b) `mod` 257

-- Multiply a and b mod 257, where 0 represents 256.
mul257 :: Word8 -> Word8 -> Word8
mul257 a b = fromIntegral (mul257' (fromIntegral a) (fromIntegral b))

invOdd = array (0,255)
  [ (i,j) | i <- [0..255], j <- [0..255], mulOdd i j == 0 ]
  :: Array Word8 Word8

inv257 = array (0,255)
  [ (i,j) | i <- [0..255], j <- [0..255], mul257 i j == 1 ]
  :: Array Word8 Word8

divOdd m n = mulOdd m (invOdd ! n)

div257 m n = mul257 m (inv257 ! n)

step x l r = mulOdd (sbox ! (mul257 x l)) r

invStep x l r = div257 (invSbox ! (divOdd x r)) l

data Daphne = Daphne (Seq.Seq Word8) (Seq.Seq Word8) deriving (Show)
-- The first Seq is the key; the second is the shift register.

emptyDaphne = Daphne Seq.Empty Seq.Empty

keyDaphne :: [Word8] -> Daphne
keyDaphne key = Daphne (Seq.fromList key) (Seq.replicate (length key) 0)
