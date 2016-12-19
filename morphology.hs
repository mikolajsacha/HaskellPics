module Morphology (MorphShape, erosion, dilation, rgbMorphology) where

import qualified Data.Array.Repa as R
import Data.Array.Repa (U, D, Z (..), (:.)(..))
import Pixel
import PixelTraversals

data MorphShape = Square | Circle | Cross deriving (Eq, Read)

reshape :: MorphShape -> Int -> [a] -> [a]
reshape Circle n li =
 map fst (filter f (zip li [0..((n*2 + 1) ^ 2) - 1]))
  where s = n*2 + 1
        f (el, i) =
          let row = quot i s
              rel_row = if row > n then s - 1 - row else row
              start_i = n - rel_row
              count = rel_row*2 + 1
              rel_i = i `mod` s in
          rel_i >= start_i && rel_i < start_i + count

reshape Square n li = take ((n*2 + 1) ^ 2) li
reshape Cross n li = 
 map fst (filter f (zip li [0..((n*2 + 1) ^ 2) - 1]))
  where s = n*2 + 1
        f (el, i) =
          let row = quot i s
              rel_i = i `mod` s in
          row == n || (rel_i == n)

basicMorphology :: (Ord a, Bounded a) => ([a] -> a) -> MorphShape -> Int -> R.DIM2 -> (R.DIM2 -> a) -> R.DIM2 -> a
basicMorphology aggregation shape n dim f coords = 
  aggregation (map (min maxBound) surr)
  where surr = reshape shape n (pixelSurrounding n dim f coords)

erosion :: (Ord a, Bounded a) => MorphShape -> Int -> R.DIM2 -> (R.DIM2 -> a) -> R.DIM2 -> a
erosion = basicMorphology minimum

dilation :: (Ord a, Bounded a) => MorphShape -> Int -> R.DIM2 -> (R.DIM2 -> a) -> R.DIM2 -> a
dilation = basicMorphology maximum

rgbMorphology :: (Ord a, Bounded a) =>
  (R.DIM2 -> (R.DIM2 -> a) -> R.DIM2 -> a) -> R.DIM2 -> (R.DIM2 -> (a, a, a)) -> R.DIM2 -> (a, a, a) 
rgbMorphology fun n f coords = (r, g, b)
  where r = fun n (fst' . f) coords
        g = fun n (snd' . f) coords
        b = fun n (trd' . f) coords

data StructElement = Zero | X | One deriving (Eq)
instance Bounded StructElement where
  minBound = Zero
  maxBound = One

fitsBasicShape :: (Eq a, Bounded a) => [StructElement] -> [a] -> Bool
fitsBasicShape sli li = all (==True) (zipWith comp sli li)
  where comp Zero el = el == maxBound
        comp One el = el == maxBound
        comp X el = True

hitAndMiss :: (Eq a, Bounded a) => [StructElement] -> Int -> R.DIM2 -> (R.DIM2 -> a) -> R.DIM2 -> a
hitAndMiss shape n dim f coords =
  if fitsBasicShape shape surr then maxBound
  else minBound
    where surr = pixelSurrounding n dim f coords


