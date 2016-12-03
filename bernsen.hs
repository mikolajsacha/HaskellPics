-- Bernsen algorithm for binarization

module Bernsen (binarize) where

import qualified Data.Array.Repa as R
import qualified Data.Map as Map
import Data.Array.Repa (U, D, Z (..), (:.)(..))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as Vm
import qualified Data.List as L
import PixelTraversals

thresholds dim arr = R.traverse arr id f
  where f lookUp coords =
          surroundingFilter minMaxMean dim lookUp coords

minMaxMean li = (L.minimum li +  L.maximum li) / 2.0

binarize dim arr = R.traverse2 arr thArr (\a b -> a) fun
  where thArr = thresholds dim arr
        fun lookUp1 lookUp2 coords =
          if lookUp1 coords >= lookUp2 coords then (maxBound, maxBound, maxBound) 
          else (0, 0, 0)
            
