-- this transformations are already implemented in JuicyPixels. I write them as an exercise

module ColorSpaces (RGB8, y, cb, cr) where

import Codec.Picture (Pixel8)
import Pixel

type RGB8 = (Pixel8, Pixel8, Pixel8)
type YCbCr = (Double, Double, Double)
type ConvFactors = (Double, Double, Double)

fromRgb :: ConvFactors -> Double -> RGB8 -> Double
fromRgb (xr, xg, xb) x0 rgb = x0 + (xr * r' + xg * g' + xb * b') / 256.0
            where (r', g', b') = fromIntegral' rgb

toRgb :: ConvFactors -> Double -> YCbCr -> Pixel8
toRgb (xy, xcb, xcr) x0 (y', cb', cr') =
  assertBounded $ round $ x0 + (xy * y' + xcb * cb' + xcr * cr') / 256.0

y :: RGB8 -> Double
y  = fromRgb ( 65.738, 129.057,  25.064) 16.0

cb :: RGB8 -> Double
cb = fromRgb (-37.945, -74.494, 112.439) 128.0

cr :: RGB8 -> Double
cr = fromRgb (112.438, -94.151, -18.285) 128.0

r :: YCbCr -> Pixel8
r = toRgb (298.082,      0.0, 408.583) (-222.921)

g :: YCbCr -> Pixel8
g = toRgb (298.082, -100.291, 208.120)   135.576

b :: YCbCr -> Pixel8
b = toRgb (298.082,  516.412,     0.0) (-276.836)

