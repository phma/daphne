module Cryptanalysis
  ( concoctShiftRegister
  , decryptOne
  , chosenCiphertext
  , ciphertextBoth
  , chosenPlainSdevs
  ) where

import Data.Word
import Data.Bits
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))
import Data.Foldable (toList,foldl')
import Control.Parallel
import Control.Parallel.Strategies
import Cryptography.Daphne.Internals
import Cryptography.Daphne
import Stats

{- This chosen-ciphertext attack consists of feeding a Daphne at least 16 MB
 - of 0x00 and 0x01 bytes and remembering what plaintext byte comes out for
 - every combination of shift register and accumulator.
 -
 - concoctShiftRegister takes a number and produces a shift register with 16
 - bits of the number spread out one bit per byte.
 -
 - The key and shift register are made of 0x00 and 0x01 because those are the
 - identity operations of mulOdd and mul257, respectively.
 -}

concoctSR :: Int -> Int -> Seq.Seq Word8 -> Seq.Seq Word8
concoctSR bits 0 sr = sr
concoctSR bits n sr = ((fromIntegral bits) .&. 1) <| (concoctSR (shift bits (-1)) (n-1) sr)

concoctShiftRegister :: Int -> Seq.Seq Word8
concoctShiftRegister bits = concoctSR bits 16 Seq.Empty

decryptOne :: Seq.Seq Word8 -> Int -> Word16
-- Takes 24 bits, accumulator and shift register, and returns 16 bits,
-- decryption of 0x00 and 0x01.
decryptOne key accBits = (fromIntegral plainOne*256)+fromIntegral plainZero
  where sreg = concoctShiftRegister accBits
	acc = fromIntegral (shift accBits (-16))
	left = computeLeft key sreg acc
	right = computeRight key sreg acc
	plainZero = invStep 0 left right
	plainOne = invStep 1 left right

chosenCiphertext :: IO ()
-- This runs through all 16M combinations of ciphertext and accumulator
-- and subjects the resulting plaintext to an avalanche test. The results are
-- a list of 24 lists of 16 numbers, which should not be much bigger than 4096
-- in absolute value. A few numbers >8192 are okay, but >10000 is suspicious. 
chosenCiphertext = print $ sacStats $ parMap rpar (decryptOne key) [0..16777215]
  where key = concoctShiftRegister 59049

{- This chosen-plaintext attack consists of feeding the same Daphne the same
 - plaintext stream, except that two bytes at the beginning are changed so as
 - not to affect the accumulator, and comparing the ciphertexts.
 -}

xorBytes :: Word -> Word8
xorBytes w64 = fromIntegral w8
  where w32 = (w64 `xor` (w64 `shift` (-32)))
	w16 = (w32 `xor` (w32 `shift` (-16)))
	w8  = (w16 `xor` (w16 `shift` (-8)))

twoBytes :: Word8 -> Word8 -> Word16
twoBytes a b = (fromIntegral a) * 256 + (fromIntegral b)

plaintext0 = map xorBytes [0..1048575]
plaintext1 = 0x6a : 0x97 : tail (tail plaintext0)

daph59049 = keyDaphne [1,0,0,1,0,1,0,1,0,1,1,0,0,1,1,1]
ciphertext0 = snd $ listEncrypt daph59049 plaintext0
ciphertext1 = snd $ listEncrypt daph59049 plaintext1
ciphertextBoth = par ciphertext0 (zipWith twoBytes ciphertext0 ciphertext1)
chosenPlainHisto = foldl' hCount (emptyHisto 65536) ciphertextBoth
-- χ² has 65535 degrees of freedom. Mean 65535, variance 131070.
chosenPlainSdevs = ((χ² chosenPlainHisto) - 65535) / (sqrt 131070)
