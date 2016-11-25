{-# LANGUAGE TypeOperators   #-}

module PixelMaps (grayscale, onlyRed, onlyGreen, onlyBlue, negative, sepia) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Codec.Picture
import Data.Array.Repa (U, D, Z (..), (:.)(..))
import qualified Data.Array.Repa as R
import Pixel

-- all functions here have type signatures:
-- f :: (R.DIM2 -> RGB8) -> R.DIM2 -> RGB8

grayscale f (Z :. i :. j) = 
  (avg, avg, avg) 
  where (r, g, b) = fromIntegral' $ f (Z :. i :. j)
        avg = round ((r + g + b) / 3)
        
onlyRed   f (Z :. i :. j) = (r, 0, 0) where (r, g, b) = f (Z :. i :. j)
onlyGreen f (Z :. i :. j) = (0, g, 0) where (r, g, b) = f (Z :. i :. j)
onlyBlue  f (Z :. i :. j) = (0, 0, b) where (r, g, b) = f (Z :. i :. j)
negative  f (Z :. i :. j) = (255 - r, 255 - g, 255 - b) where (r, g, b) = f (Z :. i :. j)

sepia f (Z :. i :. j) = 
    (assertPixel' . round') (r', g', b')
    where (r, g, b) = fromIntegral' $ f (Z :. i :. j)
          r' = r * 0.393 + g * 0.769 + b * 0.189
          g' = r * 0.349 + g * 0.686 + b * 0.168
          b' = r * 0.272 + g * 0.534 + b * 0.131
