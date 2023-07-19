module Cryptanalysis
  ( concoctShiftRegister
  , decryptOne
  ) where

import Data.Word
import Data.Bits
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))
import Data.Foldable (toList)
import Cryptography.Daphne.Internals
import Cryptography.Daphne
import Stats

{- This chosen-ciphertext attack consists of feeding a Daphne at least 16 MB
 - of 0x00 and 0x01 bytes and remembering what plaintext byte comes out for
 - every combination of shift register and accumulator.
 -
 - concoctShiftRegister takes a number and produces a shift register with 16
 - bits of the number spread out one bit per byte.
 -}

concoctSR :: Int -> Int -> Seq.Seq Word8 -> Seq.Seq Word8
concoctSR bits 0 sr = sr
concoctSR bits n sr = ((fromIntegral bits) .&. 1) <| (concoctSR (shift bits (-1)) (n-1) sr)

concoctShiftRegister :: Int -> Seq.Seq Word8
concoctShiftRegister bits = concoctSR bits 16 Seq.Empty

decryptOne :: Seq.Seq Word8 -> Int -> Int
-- Takes 24 bits, accumulator and shift register, and returns 16 bits,
-- decryption of 0x00 and 0x01.
decryptOne key accBits = (fromIntegral plainOne*256)+fromIntegral plainZero
  where sreg = concoctShiftRegister accBits
	acc = fromIntegral (shift accBits (-16))
	left = computeLeft key sreg acc
	right = computeRight key sreg acc
	plainZero = invStep 0 left right
	plainOne = invStep 1 left right

