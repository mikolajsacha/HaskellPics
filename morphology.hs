module Morphology (MorphShape, erosion, dilation, rgbMorphology, morphology, doubleMorphology) where

import qualified Data.Array.Repa as R
import Data.Array.Repa (U, D, Z (..), (:.)(..))
import Pixel
import PixelTraversals
import PixelMaps
import qualified YCbCr
import qualified Otsu

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

rgbMorphology :: (Ord a, Bounded a) => ((R.DIM2 -> a) -> R.DIM2 -> a) -> (R.DIM2 -> (a, a, a)) -> R.DIM2 -> (a, a, a) 
rgbMorphology fun f coords = (r, g, b)
  where r = fun (fst' . f) coords
        g = fun (snd' . f) coords
        b = fun (trd' . f) coords

morphology :: R.Array U R.DIM2 RGB8 ->
              ((R.DIM2 -> Bool) -> R.DIM2 -> Bool) ->
              IO (R.Array D R.DIM2 RGB8)
morphology arr fun = doubleMorphology arr fun id

doubleMorphology :: R.Array U R.DIM2 RGB8 ->
                    ((R.DIM2 -> Bool) -> R.DIM2 -> Bool) -> 
                    ((R.DIM2 -> Bool) -> R.DIM2 -> Bool) ->
                    IO (R.Array D R.DIM2 RGB8)
doubleMorphology arr fun1 fun2 = do
  yArr <- R.computeUnboxedP $ R.map YCbCr.y arr
  th <- Otsu.threshold yArr 256
  binarized <- R.computeUnboxedP $ R.map (binarize 0 th) arr
  afterMorph1 <- R.computeUnboxedP $ R.traverse binarized id fun1
  afterMorph2 <- R.computeUnboxedP $ R.traverse afterMorph1 id fun2
  return $ R.map (triple . boundedToBounded) afterMorph2

