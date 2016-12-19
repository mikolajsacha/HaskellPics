-- Bernsen algorithm for binarization

module Bernsen (binarize, mixedBinarize) where

import qualified Data.Array.Repa as R
import qualified Data.Map as Map
import Data.Array.Repa (U, D, Z (..), (:.)(..))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as Vm
import qualified Data.List as L
import PixelTraversals
import qualified Otsu
import YCbCr as Ycbcr
import Pixel

thresholds dim arr = R.traverse arr id f
  where f = surroundingFilter minMaxMean dim 

minMaxMean li = (L.minimum li +  L.maximum li) / 2.0

binarize :: R.Array U R.DIM2 RGB8 -> IO (R.Array D R.DIM2 RGB8)
binarize arr = do
  yArr <- R.computeUnboxedP $ R.map Ycbcr.y arr
  let dim = R.extent yArr
  thArr <- R.computeUnboxedP $ thresholds dim yArr
  return $ R.traverse2 yArr thArr const fun
  where fun lookUp1 lookUp2 coords =
          if lookUp1 coords >= lookUp2 coords then (maxBound, maxBound, maxBound) 
          else (0, 0, 0)
            
mixedBinarize :: R.Array U R.DIM2 RGB8 -> Double -> IO (R.Array D R.DIM2 RGB8)
mixedBinarize arr threshold = do
  yArr <- R.computeUnboxedP $ R.map Ycbcr.y arr
  otsu_threshold <- Otsu.threshold yArr 256
  let dim = R.extent yArr
  thArr <- R.computeUnboxedP $ thresholds dim yArr
  return $ R.traverse2 yArr thArr const (fun otsu_threshold)
  where fun otTh lookUp1 lookUp2 coords = 
          let diff = lookUp1 coords - lookUp2 coords in
            if abs diff > threshold then 
              if diff >= otTh then (maxBound, maxBound, maxBound)
              else (0, 0, 0)
            else 
              if diff >= 0 then (maxBound, maxBound, maxBound)
              else (0, 0, 0)
