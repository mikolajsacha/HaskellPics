module YCbCr (YCbCr, toYcbcr, fromYcbcr, y, cb, cr, r, g, b) where

import Codec.Picture (Pixel8)
import Pixel

type YCbCr = (Double, Double, Double)
type ConvFactors = (Double, Double, Double)

y :: RGB8 -> Double
y rgb = 0.299 * r' + 0.587 * g' + 0.114 * b' 
  where (r', g', b') = fromIntegral' rgb

cb :: RGB8 -> Double
cb rgb@(r, g, b) = 0.492 * (b' - y rgb) 
  where b' = fromIntegral b

cr :: RGB8 -> Double
cr rgb@(r, g, b) = 0.877 * (r' - y rgb) 
  where r' = fromIntegral r

r :: YCbCr -> Pixel8
r (y, cb, cr) = (assertBounded . round) (y + 1.14 * cr)

g :: YCbCr -> Pixel8
g (y, cb, cr) = (assertBounded . round) (y - 0.395 * cb - 0.581 * cr)

b :: YCbCr -> Pixel8
b (y, cb, cr) = (assertBounded . round) (y + 2.033 * cb)

toYcbcr :: RGB8 -> YCbCr
toYcbcr rgb = (y rgb, cb rgb, cr rgb)

fromYcbcr :: YCbCr -> RGB8
fromYcbcr ycbcr = (r ycbcr, g ycbcr, b ycbcr)

