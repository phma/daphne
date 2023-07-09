module Stats
  ( Histo (..)
  , emptyHisto
  , hCount
  , sacCountBit
  ) where

import Data.Word
import Data.Bits
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))
import Data.Foldable (toList)

data Histo = Histo (Seq.Seq Word) deriving (Show)

emptyHisto :: Int -> Histo
emptyHisto n = Histo (Seq.replicate n 0)

hCount :: Int -> Histo -> Histo
hCount n (Histo h) = Histo (Seq.adjust' (+1) n h)

squareWave :: Int -> [Int]
squareWave n = map (((-1) ^) . (.&. (1::Int)) . (`shift` (-n))) [0..]

sacCountBit :: Histo -> Int -> Int
-- The histogram is produced by a strict avalanche criterion count; i.e. each
-- number counted is the xor of two outputs whose inputs differ by one bit.
sacCountBit (Histo h) n = sum $ zipWith (*) (squareWave n) (map fromIntegral (toList h))
