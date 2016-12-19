-- otsu algorithm for binarization

module Otsu (threshold, binarize) where

import qualified Data.Array.Repa as R
import qualified Data.Map as Map
import Data.Array.Repa (U, D, Z (..), (:.)(..))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as Vm
import qualified Data.List as L
import qualified PixelMaps as PMaps
import Pixel
import qualified YCbCr

histogram :: R.Array U sh Double -> Int -> IO [Int]
histogram arr buckets = do
  hist <- Vm.replicate buckets 0
  V.mapM_ (updateHist hist) vec
  frozen <- V.unsafeFreeze hist
  return $ V.toList frozen
  where vec = R.toUnboxed arr
        max = V.maximum vec
        min = V.minimum vec
        d   = max - min + 1.0
        updateHist vec val = do
          let bucket = floor ((val - min) / d * fromIntegral buckets)
          Vm.modify vec (+1) bucket

threshold :: R.Array U sh Double -> Int -> IO Double
threshold arr buckets = do
  hist <- histogram arr buckets
  return $ histThreshold hist  

binarize :: R.Array U R.DIM2 RGB8 -> IO (R.Array D R.DIM2 RGB8)
binarize arr = do
  yArr <- R.computeUnboxedP $ R.map YCbCr.y arr
  th <- threshold yArr 256
  return $ R.map (PMaps.binarizeY' 0 th) yArr
    
histThreshold :: [Int] -> Double
histThreshold hist = (th1 + th2) / 2.0
  where hist' = map fromIntegral hist
        (wB, wF, th1, th2, sumB, max, break) =
          foldl f (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, False) (zip [0.0..] hist')
        total = fromIntegral $ L.sum hist
        sum = foldl (\acc (i, v) -> acc + i*v ) 0.0 (zip [1.0..] (L.tail hist'))
        f (wB, wF, th1, th2, sumB, max, break) (index, bucketValue) =
          if break then (wB, wF, th1, th2, sumB, max, True)
          else
            let wB' = wB + bucketValue
                wF' = total - wB' in
              if wB' == 0.0 then (wB', wF, th1, th2, sumB, max, False)
              else
                if wF' == 0.0 then (wB', wF', th1, th2, sumB, max, True)
                else
                  let sumB' = sumB + index * bucketValue in
                    fInter (wB', wF', th1, th2, sumB', max) (index, bucketValue)
        fInter (wB, wF, th1, th2, sumB, max) (index, bucketValue) =
          let mB = sumB / wB
              mF = (sum - sumB) / wF
              between = wB * wF * (mB - mF) * (mB - mF) in
              if between >= max then
                if between > max then (wB, wF, index, index, sumB, between, False)
                else (wB, wF, index, th2, sumB, between, False)
              else (wB, wF, th1, th2, sumB, max, False)
              
