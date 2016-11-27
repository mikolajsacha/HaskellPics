module PixelTraversals (averageFilter, medianFilter) where

import Pixel
import Codec.Picture (Pixel8)
import qualified Data.Array.Repa as R
import Data.Array.Repa (U, D, Z (..), (:.)(..))
import Data.List (sort, sum)

pixelSurrounding :: R.DIM2 -> (Int, Int) -> (R.DIM2 -> a) -> [a]
pixelSurrounding (Z :. w :. h) (i, j) f = 
  [f (Z :. i :. j) | i <- [i',i''], j <- [j', j'']]
  where i'  = maximum [0, i - 1]
        i'' = minimum [w - 1, i + 1]
        j'  = maximum [0, j - 1]
        j'' = minimum [h - 1, j + 1]

surroundingFilter :: ([a] -> a) -> R.DIM2 -> (R.DIM2 -> a)
                  -> R.DIM2 -> a
surroundingFilter foldFun dim f (Z :. i :. j ) =
  foldFun $ pixelSurrounding dim (i, j) f

average :: [Pixel8] -> Pixel8
average li = fromIntegral $ quot (sum li') (length li)
  where li' = map fromIntegral li

median :: [Pixel8] -> Pixel8
median li = fromIntegral $ (sort li) !! (quot (length li) 2)

forEachInTuple :: ([a] -> a) -> [(a, a, a)] -> (a, a, a)
forEachInTuple fun li = (a, b, c)
  where a = fun $ map fst' li
        b = fun $ map snd' li
        c = fun $ map trd' li

averageFilter = surroundingFilter $ forEachInTuple average
medianFilter  = surroundingFilter $ forEachInTuple median
    

