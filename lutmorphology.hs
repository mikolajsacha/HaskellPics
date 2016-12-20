module LutMorphology (erosionLut, dilationLut, lutMorphology) where

import qualified Data.Array.Repa as R
import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Vector.Generic as VG
import qualified Data.List as L
import Data.Array.Repa (U, D, Z (..), (:.)(..))
import Pixel
import PixelTraversals
import PixelMaps
import qualified YCbCr
import qualified Otsu
import Morphology

lutIndex :: (Bounded a, Eq a) => [a] -> Int
lutIndex li = sum $ zipWith (curry f) li [0..]
  where f (el, i)
          | el == maxBound = 2 ^ i
          | otherwise = 0

allCombinations :: (Bounded a, Ord a) => Int -> [[a]]
allCombinations 0 = [[]]
allCombinations len = L.sort (f' len)
  where f' 0 = [[]]
        f' l = do
          b <- [maxBound, minBound]
          map (b :) (f' (l - 1))

lut :: (Bounded a, Ord a) => ([a] -> a) -> a -> MorphShape -> Int -> V.Vector a
lut aggregateFunc defaultVal shape n = V.fromList $ map combFits (allCombinations (s ^ 2))
  where s = n*2 + 1
        combFits comb = aggregateFunc $ zipWith fit [0..] comb
        fit i el =
          let row = quot i s
              rel_i = i `mod` s in
            case shape of
              Circle -> let rel_row = if row > n then s - 1 - row else row
                            start_i = n - rel_row
                            count = rel_row*2 + 1 in
                       if rel_i >= start_i && rel_i < start_i + count then el else defaultVal
              Cross -> if row == n || rel_i == n then el else defaultVal
              Square -> el

erosionLut :: (Bounded a, Ord a) => MorphShape -> Int -> V.Vector a
erosionLut = lut minimum maxBound

dilationLut :: (Bounded a, Ord a) => MorphShape -> Int -> V.Vector a
dilationLut = lut maximum minBound

lutMorphology :: (Ord a, Bounded a) => V.Vector a -> Int -> R.DIM2 -> (R.DIM2 -> a) -> R.DIM2 -> a
lutMorphology lut_v n dim f coords = lut_v ! index
  where index = lutIndex $ pixelSurrounding n dim f coords
