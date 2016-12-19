{-# LANGUAGE TypeOperators   #-}

module PixelMaps (grayscale,
                  onlyRed, onlyGreen, onlyBlue,
                  negative, sepia,
                  onlyY, onlyCb, onlyCr,
                  onlyH, onlyL, onlyS,
                  filterHue, filterSkin, filterRedEyes,
                  binarize, binarize', binarizeY, binarizeY',
                  boundedToBounded, triple) where


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
-- f :: RGB8 -> RGB8 

grayscale rgb = 
  (avg, avg, avg) 
  where (r, g, b) = fromIntegral' rgb
        avg = round ((r + g + b) / 3)
        
onlyRed   (r, g, b) = (r, 0, 0)
onlyGreen (r, g, b) = (0, g, 0)
onlyBlue  (r, g, b) = (0, 0, b)
negative  (r, g, b) = (maxBound - r, maxBound - g, maxBound - b)

sepia rgb = 
  (assertBounded' . round') (r', g', b')
  where (r, g, b) = fromIntegral' rgb
        r' = r * 0.393 + g * 0.769 + b * 0.189
        g' = r * 0.349 + g * 0.686 + b * 0.168
        b' = r * 0.272 + g * 0.534 + b * 0.131

onlyY  rgb = Ycbcr.fromYcbcr (y, 0.0, 0.0)
  where (y, cb, cr) = Ycbcr.toYcbcr rgb

onlyCb rgb = Ycbcr.fromYcbcr (128, cb, 0.0)
  where (y, cb, cr) = Ycbcr.toYcbcr rgb

onlyCr rgb = Ycbcr.fromYcbcr (128, 0.0, cr)
  where (y, cb, cr) = Ycbcr.toYcbcr rgb

onlyH rgb = Hls.fromHls (h, 0.5, 0.5)
  where (h, l, s) = Hls.toHls rgb

onlyL rgb = Hls.fromHls (0, l, 0)
  where (h, l, s) = Hls.toHls rgb

onlyS rgb = Hls.fromHls (h, 0.5, s)
  where (h, l, s) = Hls.toHls rgb

type Range = (Double, Double)

inRange :: Range -> Double -> Bool
inRange (minVal, maxVal) val
  | minVal <= maxVal = val >= minVal && val <= maxVal
  | otherwise        = val >= minVal || val <= maxVal

filterHls :: Range -> Range -> Range -> RGB8 -> Bool
filterHls hr lr sr rgb =
  inRange hr h && inRange lr l && inRange sr s
  where (h, l, s) = Hls.toHls rgb

filterHue :: Double -> Double -> RGB8 -> RGB8
filterHue l r rgb
  | filterHls (l, r) (0, 360) (0, 360) rgb = rgb
  | otherwise = (0, 0, 0)

filterSkin :: RGB8 -> RGB8
filterSkin rgb
  | inRange (0.5, 3.0) (l/s) && filterHls (330, 28) (0, 1) (0.2, 1) rgb = rgb
  | otherwise = (0, 0, 0)
  where (h, l, s) = Hls.toHls rgb

filterRedEyes :: RGB8 -> RGB8
filterRedEyes rgb
  | inRange (0.5, 1.5) (l/s) && filterHls (162, 7) (0.25, 1) (0.4, 1) rgb = rgb
  | otherwise = Hls.fromHls (0, l, 0)
  where (h, l, s) = Hls.toHls rgb

binarizeY :: (Bounded a) => Double -> Double -> Double -> a
binarizeY a b y
  | y >= a && y < b = minBound
  | otherwise = maxBound

binarizeY' :: (Bounded a) => Double -> Double -> Double -> (a, a, a)
binarizeY' a b y
  | y >= a && y < b  = (minBound, minBound, minBound)
  | otherwise = (maxBound, maxBound, maxBound)

binarize :: (Bounded a) => Double -> Double -> RGB8 -> a
binarize a b rgb = binarizeY a b (Ycbcr.y rgb)

binarize' :: (Bounded a) => Double -> Double -> RGB8 -> (a, a, a)
binarize' a b rgb = binarizeY' a b (Ycbcr.y rgb)

boundedToBounded :: (Bounded a, Eq a, Bounded b) => a -> b
boundedToBounded a
  | a == maxBound = maxBound
  | otherwise = minBound

triple :: a -> (a, a, a)
triple a = (a, a, a)
