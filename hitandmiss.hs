module HitAndMiss (hitAndMiss1) where

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Unboxed as RU
import Data.Array.Repa (U, D, Z (..), (:.)(..))
import Pixel
import PixelTraversals
import PixelMaps
import qualified YCbCr
import qualified Otsu
import Morphology

data StructElement = Zero | X | One deriving (Eq)
instance Bounded StructElement where
  minBound = Zero
  maxBound = One

fitsBasicShape :: (Eq a, Bounded a) => [StructElement] -> [a] -> Bool
fitsBasicShape sli li = all (==True) (zipWith comp sli li)
  where comp Zero el = el == maxBound
        comp One el = el == maxBound
        comp X el = True

hitWindow :: (Eq a, Bounded a) => [StructElement] -> Int -> R.DIM2 -> (R.DIM2 -> a) -> R.DIM2 -> a
hitWindow shape n dim f coords =
  if fitsBasicShape shape surr then maxBound
  else minBound
    where surr = pixelSurrounding n dim f coords

runHitAndMiss :: (RU.Unbox a, Eq a, Bounded a) => R.Array U R.DIM2 a -> Int -> Int -> [StructElement] -> IO (R.Array U R.DIM2 a)
runHitAndMiss arr maxIterations currentIteration shape
  | currentIteration >= maxIterations = do 
      putStrLn $ "Finished Hit and Miss after " ++ show currentIteration ++ " iterations."
      return arr
  | otherwise = do
      let n = quot ((round . sqrt . fromIntegral . length) shape) 2
      let dim = R.extent arr
      result <- R.computeUnboxedP $ R.traverse arr id (hitWindow shape n dim)
      equal <- R.equalsP result arr
      if equal then do
        putStrLn $ "Finished Hit and Miss after " ++ show currentIteration ++ " iterations."
        return result
      else runHitAndMiss result maxIterations (currentIteration + 1) shape

hitAndMiss1 :: R.Array U R.DIM2 RGB8 -> Int -> IO (R.Array D R.DIM2 RGB8)
hitAndMiss1 arr maxIterations = do
  let struct1 = [One, One, One, One, X, One, One, One, One]
  yArr <- R.computeUnboxedP $ R.map YCbCr.y arr
  th <- Otsu.threshold yArr 256
  binary <- R.computeUnboxedP $ R.map (binarize 0 th :: RGB8 -> Bool) arr
  computedBinary <- runHitAndMiss binary maxIterations 0 struct1
  return $ R.map (triple . boundedToBounded) computedBinary
