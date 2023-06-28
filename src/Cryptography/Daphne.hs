module Cryptography.Daphne
  ( Daphne (..)
  , keyDaphne
  , byteEncrypt
  , byteDecrypt
  , listEncrypt
  , listDecrypt
  ) where

import Data.Word
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))
import Data.Foldable (toList)
import Cryptography.Daphne.Internals

data Daphne = Daphne (Seq.Seq Word8) (Seq.Seq Word8) Word8 deriving (Show)
-- The first Seq is the key; the second is the shift register.

keyDaphne :: [Word8] -> Daphne
keyDaphne key = Daphne (Seq.fromList key) (Seq.replicate (length key) 0) 0

computeLeft :: Seq.Seq Word8 -> Seq.Seq Word8 -> Word8 -> Word8
computeLeft Seq.Empty Seq.Empty acc = acc
computeLeft (k:<|ks) (r:<|rs) acc = step (computeLeft ks rs acc) r k

computeRight :: Seq.Seq Word8 -> Seq.Seq Word8 -> Word8 -> Word8
computeRight Seq.Empty Seq.Empty acc = acc
computeRight (ks:|>k) (rs:|>r) acc = step (computeRight ks rs acc) k r

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

seqEncrypt :: Seq.Seq Word8 -> (Daphne,Seq.Seq Word8) -> (Daphne,Seq.Seq Word8)
seqEncrypt Seq.Empty a = a
seqEncrypt (bs:|>b) (daph,acc) = (daph2,acc2) where
  (daph1,acc1) = seqEncrypt bs (daph,acc)
  (daph2,c) = byteEncrypt daph1 b
  acc2 = acc1 |> c

listEncrypt :: Daphne -> [Word8] -> (Daphne,[Word8])
listEncrypt daph bs = (daph1,toList seq1) where
  (daph1,seq1) = seqEncrypt (Seq.fromList bs) (daph,Seq.Empty)

seqDecrypt :: Seq.Seq Word8 -> (Daphne,Seq.Seq Word8) -> (Daphne,Seq.Seq Word8)
seqDecrypt Seq.Empty a = a
seqDecrypt (bs:|>b) (daph,acc) = (daph2,acc2) where
  (daph1,acc1) = seqDecrypt bs (daph,acc)
  (daph2,c) = byteDecrypt daph1 b
  acc2 = acc1 |> c

listDecrypt :: Daphne -> [Word8] -> (Daphne,[Word8])
listDecrypt daph bs = (daph1,toList seq1) where
  (daph1,seq1) = seqDecrypt (Seq.fromList bs) (daph,Seq.Empty)
