module Morphology (erosion, dilation) where

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

erosion :: Int -> R.DIM2 -> (R.DIM2 -> Bool) -> R.DIM2 -> Bool
erosion n dim f coords = and $ filterToFitStruct n struct (zipWith (&&) struct surr)
  where surr = pixelSurrounding n dim f coords
        struct = morphStruct n

dilation :: Int -> R.DIM2 -> (R.DIM2 -> Bool) -> R.DIM2 -> Bool
dilation n dim f coords = or $ filterToFitStruct n struct (zipWith (&&) struct surr)
  where surr = pixelSurrounding n dim f coords
        struct = morphStruct n
