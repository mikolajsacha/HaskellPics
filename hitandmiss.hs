module HitAndMiss (hitAndMiss1, convexHull) where

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

fitsBasicShape :: (Eq a, Bounded a) => [a] -> [StructElement] -> Bool
fitsBasicShape li sli
  | length li /= length sli = False
  | otherwise = all (==True) (zipWith comp li sli)
      where comp el Zero = el == minBound
            comp el One = el == maxBound
            comp el X = True

hitWindow :: (Eq a, Bounded a) => Bool -> [[StructElement]] -> Int -> R.DIM2 -> (R.DIM2 -> a) -> R.DIM2 -> a
hitWindow addToOriginal structs n dim f coords
  | addToOriginal && f coords == maxBound = maxBound
  | otherwise = if any (fitsBasicShape surr) structs then maxBound
                else minBound
                  where surr = pixelSurrounding n dim f coords

runHitAndMiss :: (RU.Unbox a, Eq a, Bounded a) => Bool -> R.Array U R.DIM2 a ->
                 Int -> Int -> Int -> [[StructElement]] -> IO (R.Array U R.DIM2 a)
runHitAndMiss addToOriginal arr n maxIterations currentIteration structs
  | currentIteration >= maxIterations = do 
      putStrLn $ "Finished Hit and Miss after " ++ show currentIteration ++ " iterations."
      return arr
  | otherwise = do
      let dim = R.extent arr
      result <- R.computeUnboxedP $ R.traverse arr id (hitWindow addToOriginal structs n dim)
      equal <- R.equalsP result arr
      if equal then do
        putStrLn $ "Finished Hit and Miss after " ++ show currentIteration ++ " iterations."
        return result
      else runHitAndMiss addToOriginal result n maxIterations (currentIteration + 1) structs

iterateHitAndMiss :: Bool -> Int -> [[StructElement]] -> R.Array U R.DIM2 RGB8 -> Int -> IO (R.Array D R.DIM2 RGB8)
iterateHitAndMiss addToOriginal n structs arr maxIterations = do
  yArr <- R.computeUnboxedP $ R.map YCbCr.y arr
  th <- Otsu.threshold yArr 256
  binary <- R.computeUnboxedP $ R.map (binarize 0 th :: RGB8 -> Bool) arr
  computedBinary <- runHitAndMiss addToOriginal binary n maxIterations 0 structs
  return $ R.map (triple . boundedToBounded) computedBinary

hitAndMiss1 :: R.Array U R.DIM2 RGB8 -> Int -> IO (R.Array D R.DIM2 RGB8)
hitAndMiss1 = iterateHitAndMiss False 1 [struct1]
  where struct1 = [One, One, One, One, X, One, One, One, One]

convexHull :: R.Array U R.DIM2 RGB8 -> Int -> IO (R.Array D R.DIM2 RGB8)
convexHull = iterateHitAndMiss True 1 convexHullStructs
  where convexHullStructs = [[One, One, One, One, Zero, X, X, X, Zero],
                            [One, One, One, X, Zero, One, Zero, X, X],
                            [One, One, X, One, Zero, X, One, X, Zero],
                            [One, X, Zero, One, Zero, X, One, One, X],
                            [X, One, One, X, Zero, One, Zero, X, One],
                            [X, X, Zero, One, Zero, X, One, One, One],
                            [Zero, X, One, X, Zero, One, X, One, One],
                            [Zero, X, X, X, Zero, One, One, One, One]]
