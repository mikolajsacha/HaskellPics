{-# LANGUAGE TypeOperators   #-}

module PixelMaps (grayscale,
                  onlyRed, onlyGreen, onlyBlue,
                  negative, sepia,
                  onlyY, onlyCb, onlyCr,
                  onlyH, onlyL, onlyS,
                  filterHue, filterSkin) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Codec.Picture
import Data.Array.Repa (U, D, Z (..), (:.)(..))
import qualified Data.Array.Repa as R
import Pixel
import qualified YCbCr as Ycbcr
import qualified HLS as Hls

-- all functions below have type signatures:
-- f :: (R.DIM2 -> RGB8) -> R.DIM2 -> RGB8

grayscale f (Z :. i :. j) = 
  (avg, avg, avg) 
  where (r, g, b) = fromIntegral' $ f (Z :. i :. j)
        avg = round ((r + g + b) / 3)
        
onlyRed   f (Z :. i :. j) = (r, 0, 0) where (r, g, b) = f (Z :. i :. j)
onlyGreen f (Z :. i :. j) = (0, g, 0) where (r, g, b) = f (Z :. i :. j)
onlyBlue  f (Z :. i :. j) = (0, 0, b) where (r, g, b) = f (Z :. i :. j)
negative  f (Z :. i :. j) = (255 - r, 255 - g, 255 - b) where (r, g, b) = f (Z :. i :. j)

sepia f (Z :. i :. j) = 
  (assertBounded' . round') (r', g', b')
  where (r, g, b) = fromIntegral' $ f (Z :. i :. j)
        r' = r * 0.393 + g * 0.769 + b * 0.189
        g' = r * 0.349 + g * 0.686 + b * 0.168
        b' = r * 0.272 + g * 0.534 + b * 0.131

onlyY f (Z :. i :. j) = Ycbcr.fromYcbcr (y, 0.0, 0.0)
  where (y, cb, cr) = Ycbcr.toYcbcr $ f (Z :. i :. j)

onlyCb f (Z :. i :. j) = Ycbcr.fromYcbcr (128, cb, 0.0)
  where (y, cb, cr) = Ycbcr.toYcbcr $ f (Z :. i :. j)

onlyCr f (Z :. i :. j) = Ycbcr.fromYcbcr (128, 0.0, cr)
  where (y, cb, cr) = Ycbcr.toYcbcr $ f (Z :. i :. j)

onlyH f (Z :. i :. j) = Hls.fromHls (h, 0.5, 0.5)
  where (h, l, s) = Hls.toHls $ f (Z :. i :. j)

onlyL f (Z :. i :. j) = Hls.fromHls (0, l, 0)
  where (h, l, s) = Hls.toHls $ f (Z :. i :. j)

onlyS f (Z :. i :. j) = Hls.fromHls (h, 0.5, s)
  where (h, l, s) = Hls.toHls $ f (Z :. i :. j)

type Range = (Double, Double)

inRange :: Range -> Double -> Bool
inRange (minVal, maxVal) val
  | minVal <= maxVal = val >= minVal && val <= maxVal
  | otherwise        = val >= minVal || val <= maxVal

filterHls :: Range -> Range -> Range -> (R.DIM2 -> RGB8) -> R.DIM2 -> RGB8
filterHls hr lr sr f (Z :. i :. j)
  | inRange hr h && inRange lr l && inRange sr s = f (Z :. i :. j)
  | otherwise = (0, 0, 0)
  where (h, l, s) = Hls.toHls $ f (Z :. i :. j)

filterHue :: Range -> (R.DIM2 -> RGB8) -> R.DIM2 -> RGB8
filterHue hr = filterHls hr (0,360) (0,360)

filterSkin :: (R.DIM2 -> RGB8) -> R.DIM2 -> RGB8
filterSkin f (Z :. i :. j)
  | inRange (0.5,3.0) (l/s) = filterHls (330,28) (0,360) (0.2,1) f (Z :. i :. j)
  | otherwise = (0, 0, 0)
  where (h, l, s) = Hls.toHls $ f (Z :. i :. j)
  
