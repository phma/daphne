module Stats
  ( Histo (..)
  , emptyHisto
  , hCount
  ) where

import Data.Word
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))
import Data.Foldable (toList)

data Histo = Histo (Seq.Seq Word) deriving (Show)

emptyHisto :: Int -> Histo
emptyHisto n = Histo (Seq.replicate n 0)

hCount :: Int -> Histo -> Histo
hCount n (Histo h) = Histo (Seq.adjust' (+1) n h)
