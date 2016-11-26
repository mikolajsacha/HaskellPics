-- basic functions for operating on single pixel
module Pixel (fromIntegral', round', assertBounded, assertBounded') where

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

