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

keyDaphne :: [Word8] -> Daphne
keyDaphne key = Daphne (Seq.fromList key) (Seq.replicate (length key) 0) 0

byteEncrypt :: Daphne -> Word8 -> (Daphne,Word8)
byteEncrypt (Daphne key sreg acc) plain = ((Daphne key newsreg newacc),crypt) where
  left = computeLeft key sreg acc
  right = computeRight key sreg acc
  crypt = step plain left right
  newacc = acc+plain
  newsreg = Seq.drop 1 (sreg |> crypt)

byteDecrypt :: Daphne -> Word8 -> (Daphne,Word8)
byteDecrypt (Daphne key sreg acc) crypt = ((Daphne key newsreg newacc),plain) where
  left = computeLeft key sreg acc
  right = computeRight key sreg acc
  plain = invStep crypt left right
  newacc = acc+plain
  newsreg = Seq.drop 1 (sreg |> crypt)

listEncrypt :: Traversable t => Daphne -> t Word8 -> (Daphne,t Word8)
listEncrypt = mapAccumL byteEncrypt

listDecrypt :: Traversable t => Daphne -> t Word8 -> (Daphne,t Word8)
listDecrypt = mapAccumL byteDecrypt
