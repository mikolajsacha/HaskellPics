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

filterToFitStruct :: Int -> [a] -> [a]
filterToFitStruct n = map snd . filter fst . zip (morphStruct n :: [Bool])

erosion :: (Ord a, Bounded a) => Int -> R.DIM2 -> (R.DIM2 -> (a, a, a)) -> R.DIM2 -> (a, a, a)
erosion n dim f coords = 
  (morph fst', morph snd', morph trd')
  where surr = pixelSurrounding n dim f coords
        morph el = minimum $ filterToFitStruct n (zipWith min (morphStruct n) (map el surr))

dilation :: (Ord a, Bounded a) => Int -> R.DIM2 -> (R.DIM2 -> (a, a, a)) -> R.DIM2 -> (a, a, a)
dilation n dim f coords =
  (morph fst', morph snd', morph trd')
  where surr = pixelSurrounding n dim f coords
        morph el = maximum $ filterToFitStruct n (zipWith min (morphStruct n) (map el surr))
