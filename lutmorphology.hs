module LutMorphology where

import qualified Data.Array.Repa as R
import qualified Data.Vector as V
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

lut :: MorphShape -> Int -> [Bool]
lut Square n = replicate ((n*2 + 1) ^ 2 - 1) False ++ [True]
lut Circle n = map combFits (allCombinations (s ^ 2))
  where s = n*2 + 1
        combFits comb = all fit (zip [0..] comb)
        fit (i, el) =
          let row = quot i s
              rel_row = if row > n then s - 1 - row else row
              start_i = n - rel_row
              count = rel_row*2 + 1
              rel_i = i `mod` s in
          not (rel_i >= start_i && rel_i < start_i + count) || el

lut Cross n = map combFits (allCombinations (s ^ 2))
  where s = n*2 + 1
        combFits comb = all fit (zip [0..] comb)
        fit (i, el) =
          let row = quot i s
              rel_i = i `mod` s in
          not (row == n || (rel_i == n)) || el
