module Morphology (erosion, dilation, rgbMorphology) where

import Codec.Picture (Pixel8)
import qualified Data.Array.Repa as R
import Data.Array.Repa (U, D, Z (..), (:.)(..))
import Data.List as L
import YCbCr as Ycbcr
import Pixel
import PixelTraversals

morphStruct :: (Bounded a) => Int -> [a]
morphStruct n
  | n < 0 = error "morphStruct: n must be non-negative"
  | otherwise = iter 0
  where iter i | i == n = replicate (2*n + 1) maxBound
               | otherwise = l ++ iter (i + 1) ++ l
                 where l = zeros ++ ones ++ zeros 
                       ones = replicate (i*2 + 1) maxBound
                       zeros = replicate (n - i) minBound

filterToFitStruct :: (Eq a, Bounded a) => Int -> [a] -> [a] -> [a]
filterToFitStruct n struct = map snd . filter ((== maxBound) . fst) . zip struct

basicMorphology :: (Ord a, Bounded a) => ([a] -> a) -> Int -> R.DIM2 -> (R.DIM2 -> a) -> R.DIM2 -> a
basicMorphology aggregation n dim f coords = 
  aggregation $ filterToFitStruct n struct (zipWith min struct surr)
  where surr = pixelSurrounding n dim f coords
        struct = morphStruct n

erosion :: (Ord a, Bounded a) => Int -> R.DIM2 -> (R.DIM2 -> a) -> R.DIM2 -> a
erosion = basicMorphology minimum

dilation :: (Ord a, Bounded a) => Int -> R.DIM2 -> (R.DIM2 -> a) -> R.DIM2 -> a
dilation = basicMorphology maximum

rgbMorphology fun n dim f coords = (r, g, b)
  where r = fun n dim (fst' . f) coords
        g = fun n dim (snd' . f) coords
        b = fun n dim (trd' . f) coords
