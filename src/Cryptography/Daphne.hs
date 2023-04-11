module Cryptography.Daphne
  ( twist
  ) where

import Data.Bits
import Data.Array.Unboxed
import Data.Word
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))

twist :: Bits a => Num a => a -> a
twist n = rotate n (popCount n)

-- funSbox :: Word8 -> Word8
