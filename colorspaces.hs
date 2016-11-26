module ColorSpaces (ConvFactors, fromRgb, toRgb) where

import Codec.Picture (Pixel8)
import Pixel

type ConvFactors = (Double, Double, Double)

fromRgb :: ConvFactors -> Double -> RGB8 -> Double
fromRgb (xr, xg, xb) x0 rgb = x0 + (xr * r' + xg * g' + xb * b') / 256.0
            where (r', g', b') = fromIntegral' rgb

toRgb :: ConvFactors -> Double -> (Double, Double, Double) -> Pixel8
toRgb (xy, xcb, xcr) x0 (y', cb', cr') =
  assertBounded $ round $ x0 + (xy * y' + xcb * cb' + xcr * cr') / 256.0
