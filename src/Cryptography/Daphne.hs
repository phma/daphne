{- |
Copyright 2023 Pierre Abbat

* Daphne

Daphne is a self-synchronizing byte stream cipher. It takes an arbitrary-length
key and encrypts a stream of bytes in such a way that, if a byte is garbled,
lost, or inserted in the ciphertext, the plaintext will be garbled for the key
length plus a variable number of bytes, usually less than 256, and will then
recover.
-}
module Cryptography.Daphne
  ( Daphne (..)
  , keyDaphne
  , byteEncrypt
  , byteDecrypt
  , listEncrypt
  , listDecrypt
  ) where

import Data.Word
import Data.List
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))
import Data.Foldable (toList)
import Cryptography.Daphne.Internals

data Daphne = Daphne (Seq.Seq Word8) (Seq.Seq Word8) Word8 deriving (Show)
-- The first Seq is the key; the second is the shift register.

-- | Creates a Daphne with the given key
keyDaphne :: [Word8] -- ^ The key, typically 16 bytes but can be any length
  -> Daphne -- ^ The new Daphne with the given key
keyDaphne key = Daphne (Seq.fromList key) (Seq.replicate (length key) 0) 0

-- | Encrypts a byte. Returns the modified Daphne and the ciphertext byte.
byteEncrypt :: Daphne -> Word8 -> (Daphne,Word8)
byteEncrypt (Daphne key sreg acc) plain = ((Daphne key newsreg newacc),crypt) where
  left = computeLeft key sreg acc
  right = computeRight key sreg acc
  crypt = step plain left right
  newacc = acc+plain
  newsreg = Seq.drop 1 (sreg |> crypt)

-- | Decrypts a byte. Returns the modified Daphne and the plaintext byte.
byteDecrypt :: Daphne -> Word8 -> (Daphne,Word8)
byteDecrypt (Daphne key sreg acc) crypt = ((Daphne key newsreg newacc),plain) where
  left = computeLeft key sreg acc
  right = computeRight key sreg acc
  plain = invStep crypt left right
  newacc = acc+plain
  newsreg = Seq.drop 1 (sreg |> crypt)

-- | Encrypts a stream of bytes. Returns the modified Daphne and the ciphertext.
-- To avoid hogging memory, do not look at the modified Daphne until
-- you have processed all the ciphertext.
listEncrypt :: Traversable t => Daphne -> t Word8 -> (Daphne,t Word8)
listEncrypt = mapAccumL byteEncrypt

-- | Decrypts a stream of bytes. Returns the modified Daphne and the plaintext.
-- To avoid hogging memory, do not look at the modified Daphne until
-- you have processed all the plaintext.
listDecrypt :: Traversable t => Daphne -> t Word8 -> (Daphne,t Word8)
listDecrypt = mapAccumL byteDecrypt
