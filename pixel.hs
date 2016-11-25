-- basic functions for operating on single pixel
module Pixel (RGB8, fromIntegral', round', assertPixel, assertPixel') where

import Codec.Picture (Pixel8)

type RGB8 = (Pixel8, Pixel8, Pixel8)

-- functions with ' are equivalents of other functions working on RGB8 tuple

fromIntegral' :: (Integral a, Num b) => (a, a, a) -> (b, b, b)
fromIntegral' (r, g, b) = (fromIntegral r, fromIntegral g, fromIntegral b)

round' :: (RealFrac a, Integral b) => (a, a, a) -> (b, b, b)
round' (r, g, b) = (round r, round g, round b)

assertPixel :: (Integral a) => a -> Pixel8
assertPixel p 
  | p > 255 = 255
  | p < 0 = 0
  | otherwise = fromIntegral p

assertPixel' :: (Integral a) => (a, a, a) -> RGB8
assertPixel' (r, g, b) = (assertPixel r, assertPixel g, assertPixel b)

