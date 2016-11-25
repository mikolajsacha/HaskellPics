-- code taken from tutorial: https://www.stackbuilders.com/tutorials/haskell/image-processing/

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module JuicyRepa (RGB8, fromImage, toImage) where

import Codec.Picture
import qualified Data.Array.Repa as R
import Data.Array.Repa (U, D, Z (..), (:.)(..), (!))

type RGB8 = (Pixel8, Pixel8, Pixel8)

-- | Produce delayed Repa array from image with true color pixels.
fromImage :: Image PixelRGB8 -> R.Array D R.DIM2 RGB8
fromImage img@Image {..} =
  R.fromFunction
    (Z :. imageWidth :. imageHeight)
    (\(Z :. x :. y) ->
       let (PixelRGB8 r g b) = pixelAt img x y
       in (r, g, b))

-- | Get image with true color pixels from manifest Repa array.
toImage :: R.Array U R.DIM2 RGB8 -> Image PixelRGB8
toImage a = generateImage gen width height
  where
    Z :. width :. height = R.extent a
    gen x y =
      let (r,g,b) = a ! (Z :. x :. y)
      in PixelRGB8 r g b
