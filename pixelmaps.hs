{-# LANGUAGE TypeOperators   #-}

module PixelMaps (grayscale, onlyRed, onlyGreen, onlyBlue) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Codec.Picture
import Data.Array.Repa (U, D, Z (..), (:.)(..))
import qualified Data.Array.Repa as R
import JuicyRepa (RGB8)

-- all functions here have type signature:
-- f :: (R.DIM2 -> RGB8) -> R.DIM2 -> RGB8
--
grayscale f (Z :. i :. j) = 
  (avg, avg, avg) 
  where (r, g, b) = f (Z :. i :. j)
        r' = fromIntegral r
        g' = fromIntegral g
        b' = fromIntegral b
        avg = round ((r' + g' + b') / 3)
        
onlyRed f (Z :. i :. j) = 
  (r, 0, 0) where (r, g, b) = f (Z :. i :. j)

onlyGreen f (Z :. i :. j) = 
  (0, g, 0) where (r, g, b) = f (Z :. i :. j)

onlyBlue f (Z :. i :. j) = 
  (0, 0, b) where (r, g, b) = f (Z :. i :. j)
