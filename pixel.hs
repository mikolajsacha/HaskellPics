-- basic functions for operating on a single pixel
module Pixel (RGB8, fromIntegral', round', assertBounded, assertBounded') where

import Codec.Picture (Pixel8)

type RGB8 = (Pixel8, Pixel8, Pixel8)

-- functions with ' are equivalents of other functions working on RGB8 tuple

fromIntegral' :: (Integral a, Num b) => (a, a, a) -> (b, b, b)
fromIntegral' (r, g, b) = (fromIntegral r, fromIntegral g, fromIntegral b)

round' :: (RealFrac a, Integral b) => (a, a, a) -> (b, b, b)
round' (r, g, b) = (round r, round g, round b)

assertBounded :: (Integral a, Integral b, Bounded b) => a -> b
assertBounded p 
  | p > fromIntegral max = max
  | p < fromIntegral min = min
  | otherwise = fromIntegral p
  where max = maxBound
        min = minBound

assertBounded' :: (Integral a, Integral b, Bounded b) => (a, a, a) -> (b, b, b)
assertBounded' (r, g, b) = (assertBounded r, assertBounded g, assertBounded b)

