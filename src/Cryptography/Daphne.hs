module Cryptography.Daphne
  ( twist
  , sbox
  , invSbox
  ) where

import Data.Bits
import Data.Array.Unboxed
import Data.Word
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))

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
