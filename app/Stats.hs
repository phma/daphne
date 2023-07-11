module Stats
  ( Histo (..)
  , emptyHisto
  , hCount
  , sacCountBit
  , bitFold
  ) where

import Data.Word
import Data.Bits
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))
import Data.Foldable (toList)

newtype Histo = Histo (Seq.Seq Word) deriving (Show)

emptyHisto :: Int -> Histo
emptyHisto n = Histo (Seq.replicate n 0)

hCount :: Int -> Histo -> Histo
hCount n (Histo h) = Histo (Seq.adjust' (+1) n h)

squareWave :: Int -> [Int]
squareWave n = map ((.&. (1::Int)) . (`shift` (-n))) [0..]

squareWaveInt :: Int -> [Int]
squareWaveInt n = map ((-1) ^) (squareWave n)

squareWaveBool :: Int -> [Bool]
squareWaveBool n = map (== 1) (squareWave n)

sacCountBit :: Histo -> Int -> Int
-- The histogram is produced by a strict avalanche criterion count; i.e. each
-- number counted is the xor of two outputs whose inputs differ by one bit.
sacCountBit (Histo h) n = sum $ zipWith (*) (squareWaveInt n) (map fromIntegral (toList h))

bitFoldSq :: Bits a => [a] -> [Bool] -> Seq.Seq a -> [a]
bitFoldSq [] _ _ = []
bitFoldSq _ [] _ = []
bitFoldSq (x:xs) (False:bs) as = bitFoldSq xs bs (as |> x)
bitFoldSq (x:xs) (True:bs) Seq.Empty = x:bitFoldSq xs bs Seq.Empty -- should never happen
bitFoldSq (x:xs) (True:bs) (a:<|as) = (xor x a):bitFoldSq xs bs as

bitFold :: Bits a => [a] -> Int -> [a]
bitFold xs n = bitFoldSq xs (squareWaveBool n) Seq.Empty
