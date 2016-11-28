module PixelTraversals (averageFilter, medianFilter,
                        yMedianFilter, yAverageFilter) where

import Codec.Picture (Pixel8)
import qualified Data.Array.Repa as R
import Data.Array.Repa (U, D, Z (..), (:.)(..))
import Data.List (sort, sum)
import YCbCr as Ycbcr
import Pixel

pixelSurrounding :: R.DIM2 -> (R.DIM2 -> a) -> R.DIM2 -> [a]
pixelSurrounding (Z :. w :. h) f (Z :. i :. j) = 
  [f (Z :. i :. j) | i <- [i',i''], j <- [j', j'']]
  where i'  = maximum [0, i - 1]
        i'' = minimum [w - 1, i + 1]
        j'  = maximum [0, j - 1]
        j'' = minimum [h - 1, j + 1]

surroundingFilter :: ([a] -> a) -> R.DIM2 -> (R.DIM2 -> a)
                  -> R.DIM2 -> a
surroundingFilter foldFun dim f coords = foldFun (pixelSurrounding dim f coords)

average :: [Pixel8] -> Pixel8
average li = fromIntegral $ quot (sum li') (length li)
  where li' = map fromIntegral li

averageD :: [Double] -> Double
averageD li = (sum li) / (fromIntegral (length li))

median :: (Ord a) => [a] -> a
median li = (sort li) !! (quot (length li) 2)

forEachInTuple :: ([a] -> a) -> [(a, a, a)] -> (a, a, a)
forEachInTuple fun li = (a, b, c)
  where a = fun $ map fst' li
        b = fun $ map snd' li
        c = fun $ map trd' li

averageFilter :: R.DIM2 -> (R.DIM2 -> RGB8) -> R.DIM2 -> RGB8
averageFilter = surroundingFilter $ forEachInTuple average

medianFilter :: R.DIM2 -> (R.DIM2 -> RGB8) -> R.DIM2 -> RGB8
medianFilter  = surroundingFilter $ forEachInTuple median

yAverageFilter :: R.DIM2 -> (R.DIM2 -> RGB8) -> R.DIM2 -> RGB8
yAverageFilter dim f coords = Ycbcr.fromYcbcr (y', cb, cr)
  where (y, cb, cr) = Ycbcr.toYcbcr $ f coords
        y' = surroundingFilter averageD dim (Ycbcr.y . f) coords
          
yMedianFilter :: R.DIM2 -> (R.DIM2 -> RGB8) -> R.DIM2 -> RGB8
yMedianFilter dim f coords = Ycbcr.fromYcbcr (y', cb, cr)
  where (y, cb, cr) = Ycbcr.toYcbcr $ f coords
        y' = surroundingFilter median dim (Ycbcr.y . f) coords
