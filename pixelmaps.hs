{-# LANGUAGE TypeOperators   #-}

module PixelMaps (grayscale) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Codec.Picture
import Data.Array.Repa (U, D, Z (..), (:.)(..))
import qualified Data.Array.Repa as R
import JuicyRepa (RGB8)

grayscale :: (R.DIM2 -> RGB8) -> R.DIM2 -> RGB8
grayscale f (Z :. i :. j) = 
  (avg, avg, avg) 
  where (a, b, c) = f (Z :. i :. j)
        a' = fromIntegral a
        b' = fromIntegral b
        c' = fromIntegral c
        avg = round ((a' + b' + c') / 3)
        
