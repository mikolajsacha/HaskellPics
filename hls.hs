module HLS (HLS, toHls, fromHls, h, l, s, r, g, b) where

import Codec.Picture (Pixel8)
import Pixel

type HLS = (Double, Double, Double)

normalizeRgb :: RGB8 -> (Double, Double, Double)
normalizeRgb rgb = (r / 255.0, g / 255.0, b / 255.0)
  where (r, g, b) = fromIntegral' rgb

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
fromHls (h, l, s)
  | h' < 1 = normalize (c, x, 0)
  | h' < 2 = normalize (x, c, 0)
  | h' < 3 = normalize (0, c, x)
  | h' < 4 = normalize (0, x, c)
  | h' < 5 = normalize (x, 0, c)
  | otherwise = normalize (c, 0, x) 
  where c = (1.0 - abs (2.0 * l - 1.0)) * s
        h' = floor (h / 60.0)
        x = c * fromIntegral (1 - abs (h' `mod` 2 - 1))
        m = l - c * 0.5
        addm a = round (255.0 * (a + m))
        normalize (a, b, c) = assertBounded' (addm a, addm b, addm c)


