-- Bernsen algorithm for binarization

module Bernsen (binarize, mixedBinarize) where

import qualified Data.Array.Repa as R
import qualified Data.Map as Map
import Data.Array.Repa (U, D, Z (..), (:.)(..))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as Vm
import qualified Data.List as L
import PixelTraversals
import Otsu
import YCbCr as Ycbcr
import Pixel

thresholds dim arr = R.traverse arr id f
  where f = surroundingFilter minMaxMean dim 

minMaxMean li = (L.minimum li +  L.maximum li) / 2.0

binarize dim arr = R.traverse2 arr thArr const fun
  where thArr = thresholds dim arr
        fun lookUp1 lookUp2 coords =
          if lookUp1 coords >= lookUp2 coords then (maxBound, maxBound, maxBound) 
          else (0, 0, 0)
            
mixedBinarize :: R.Array U R.DIM2 Double -> Double -> IO (R.Array D R.DIM2 RGB8)
mixedBinarize yArr threshold = do
  otsu_threshold <- Otsu.threshold yArr 256
  let dim = R.extent yArr
  let thArr = thresholds dim yArr
  return $ R.traverse2 yArr thArr const (fun otsu_threshold)
  where fun otTh lookUp1 lookUp2 coords = 
          let diff = lookUp1 coords - lookUp2 coords in
            if abs diff > threshold then 
              if diff >= otTh then (maxBound, maxBound, maxBound)
              else (0, 0, 0)
            else 
              if diff >= 0 then (maxBound, maxBound, maxBound)
              else (0, 0, 0)
