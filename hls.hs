module HLS (HLS, toHls, fromHls, h, l, s, r, g, b) where

import Codec.Picture (Pixel8)
import Pixel

type HLS = (Double, Double, Double)

normalizeRgb :: RGB8 -> (Double, Double, Double)
normalizeRgb rgb = (r / 255.0, g / 255.0, b / 255.0)
  where (r, g, b) = fromIntegral' rgb

unpackCoord :: Double -> Pixel8
unpackCoord x = (assertBounded . round) (x * 255.0)

unpackRgb :: HLS -> RGB8
unpackRgb (r, g, b) = (unpackCoord r, unpackCoord g, unpackCoord b)

h :: RGB8 -> Double
h rgb = h' where (h', l', s') = toHls rgb

l :: RGB8 -> Double
l rgb = l' where (h', l', s') = toHls rgb

s :: RGB8 -> Double
s rgb = s' where (h', l', s') = toHls rgb

r :: HLS -> Pixel8
r hls = r' where (r', g', b') = fromHls hls

g :: HLS -> Pixel8
g hls = g' where (r', g', b') = fromHls hls

b :: HLS -> Pixel8
b hls = b' where (r', g', b') = fromHls hls

toHls :: RGB8 -> HLS
toHls rgb = (h', l', s')
  where (r, g, b) = normalizeRgb rgb
        max = max' (r, g, b)
        min = min' (r, g, b)
        d = max - min
        l' = (max + min) / 2.0
        s' | l' > 0.5 = d / (2.0 - max - min)
           | otherwise = d / (max + min)
        h' | d == 0.0 = 0.0
           | r == max = 60.0 * ((g - b) / d)
           | g == max = 60.0 * ((b - r) / d + 2.0)
           | otherwise = 60.0 * ((r - g) / d + 4.0)

fromHls :: HLS -> RGB8
fromHls (h, l, s) = unpackRgb (r, g, b)
  where hue2rgb p q t  
          | t' < 1.0/6.0 = p + (q - p) * 6.0 * t'
          | t' < 1.0/2.0 = q
          | t' < 2.0/3.0 = p + (q - p) * (2.0/3.0 - t') * 6.0
          | otherwise = p
          where t' | t < 0 = t + 1
                   | t > 1 = t - 1
                   | otherwise = t
        q' | l < 0.5 = l * (1.0 + s)
           | otherwise = l + s - l * s 
        p' = 2 * l - q'
        r = hue2rgb p' q' (h/360.0 + 1.0/3.0)
        g = hue2rgb p' q' (h/360.0)
        b = hue2rgb p' q' (h/360.0 - 1.0/3.0)
  



