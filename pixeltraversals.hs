module PixelTraversals (averageFilter, medianFilter,
                        yMedianFilter, yAverageFilter,
                        pixelSurrounding, surroundingFilter) where

import Codec.Picture (Pixel8)
import qualified Data.Array.Repa as R
import Data.Array.Repa (U, D, Z (..), (:.)(..))
import Data.List (sort, sum)
import YCbCr as Ycbcr
import Pixel

pixelSurrounding :: Int -> R.DIM2 -> (R.DIM2 -> a) -> R.DIM2 -> [a]
pixelSurrounding n (Z :. w :. h) f (Z :. i :. j)
  | n < 0 = error "pixelSurrounding: n must be non-negative"
  | n == 0 = [f (Z :. i :. j)]
  | otherwise = 
    [f (Z :. i :. j) | i <- [i'..i''], j <- [j'..j'']]
    where i'  = maximum [0, i - n]
          i'' = minimum [w - 1, i + n]
          j'  = maximum [0, j - n]
          j'' = minimum [h - 1, j + n]

surroundingFilter :: ([a] -> a) -> R.DIM2 -> (R.DIM2 -> a)
                  -> R.DIM2 -> a
surroundingFilter foldFun dim f coords = foldFun (pixelSurrounding 1 dim f coords)

average :: [Pixel8] -> Pixel8
average li = fromIntegral $ quot (sum li') (length li)
  where li' = map fromIntegral li

averageD :: [Double] -> Double
averageD li = sum li / fromIntegral (length li)

median :: (Ord a) => [a] -> a
median li = sort li !! quot (length li) 2

forEachInTuple :: ([a] -> a) -> [(a, a, a)] -> (a, a, a)
forEachInTuple fun li = (a, b, c)
  where a = fun $ map fst' li
        b = fun $ map snd' li
        c = fun $ map trd' li

averageFilter :: R.DIM2 -> (R.DIM2 -> RGB8) -> R.DIM2 -> RGB8
averageFilter = surroundingFilter $ forEachInTuple average

medianFilter :: R.DIM2 -> (R.DIM2 -> RGB8) -> R.DIM2 -> RGB8
medianFilter  = surroundingFilter $ forEachInTuple median

yAverageFilter :: R.DIM2 -> (R.DIM2 -> Ycbcr.YCbCr) -> R.DIM2 -> Ycbcr.YCbCr
yAverageFilter dim f coords = (y', cb, cr)
  where (y, cb, cr) = f coords
        y' = surroundingFilter averageD dim (fst' . f) coords
          
yMedianFilter :: R.DIM2 -> (R.DIM2 -> Ycbcr.YCbCr) -> R.DIM2 -> Ycbcr.YCbCr
yMedianFilter dim f coords = (y', cb, cr)
  where (y, cb, cr) = f coords
        y' = surroundingFilter median dim (fst' . f) coords
