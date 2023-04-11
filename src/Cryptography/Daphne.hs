module Cryptography.Daphne
  ( twist
  , funSbox
  ) where

import Data.Bits
import Data.Array.Unboxed
import Data.Word
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))

twist :: Bits a => Int -> a -> a
twist k n = rotate n ((popCount n)*k)

funSbox :: Word8 -> Word8
funSbox = (xor 0x6e) . (twist (-1)) . (xor 0x25)
