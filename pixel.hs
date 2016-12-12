-- basic functions for operating on a single pixel
module Pixel (RGB8, fromIntegral', round', assertBounded, assertBounded', max', min', fst', snd', trd', map') where

import Codec.Picture (Pixel8)

type RGB8 = (Pixel8, Pixel8, Pixel8)

-- functions with ' are equivalents of other functions working on a tuple

fst' (a, b, c) = a
snd' (a, b, c) = b
trd' (a, b, c) = c

fromIntegral' :: (Integral a, Num b) => (a, a, a) -> (b, b, b)
fromIntegral' = map' fromIntegral

round' :: (RealFrac a, Integral b) => (a, a, a) -> (b, b, b)
round' = map' round

assertBounded :: (Integral a, Integral b, Bounded b) => a -> b
assertBounded p 
  | p > fromIntegral max = max
  | p < fromIntegral min = min
  | otherwise = fromIntegral p
  where max = maxBound
        min = minBound

assertBounded' :: (Integral a, Integral b, Bounded b) => (a, a, a) -> (b, b, b)
assertBounded' = map' assertBounded

max' :: (Ord a) => (a, a, a) -> a
max' (x, y, z) = maximum [x, y, z]

min' :: (Ord a) => (a, a, a) -> a
min' (x, y, z) = minimum [x, y, z]

map' :: (a -> b) -> (a, a, a) -> (b, b, b)
map' f (a, b, c) = (f a, f b, f c)
