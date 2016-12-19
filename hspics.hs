{-# LANGUAGE TypeOperators   #-}

module Main (main) where

import Codec.Picture
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import System.Process (system)
import qualified Codec.Picture.Types as M
import qualified Data.Array.Repa as R
import Data.Array.Repa (U, D, Z (..), (:.)(..))
import qualified Data.Array.Repa.Repr.Unboxed as RU
import qualified Data.Vector as V
import Data.Char (toLower)
import qualified Criterion.Measurement as Cr
import PixelMaps
import PixelTraversals
import JuicyRepa
import YCbCr (toYcbcr, fromYcbcr, y)
import qualified Otsu
import qualified Bernsen
import qualified Morphology 
import qualified HitAndMiss

outputPath = "output.png"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "No command provided"
    [x] -> putStrLn "No input array provided"
    (cmd:imgPath:params) -> runCommandHandleExceptions imgPath cmd params

runCommandHandleExceptions :: String -> String -> [String] -> IO ()
runCommandHandleExceptions imgPath cmd params =
  catch commandRun handler
  where
    commandRun = do
      Cr.initializeTime
      maybeArr <- runMaybeT $ loadImg imgPath
      case maybeArr of
        Nothing -> putStrLn "Loading image failed"
        Just arr -> do 
          result <- runMaybeT $ runCommand arr cmd params
          case result of
            Nothing -> putStrLn "Command failed"
            Just resultImg -> do
              saveArray resultImg
              executionTime <- Cr.getTime
              putStr "Command succedded in "
              (putStr . show . round . (*1000)) executionTime
              putStrLn " milliseconds"
              void $ system outputPath
    handler :: SomeException -> IO ()
    handler _ = putStrLn "Probably wrong command format. Please check out README."

readImg :: FilePath -> MaybeT IO (Image PixelRGB8)
readImg path = do
  img <- liftIO $ readImage path
  case img of
    Left err -> do 
      liftIO $ putStrLn $ "Could not read image " ++ path ++ ": " ++ err
      MaybeT $ return Nothing
    Right img -> MaybeT (return $ Just $ convertRGB8 img)

loadImg :: String -> MaybeT IO (R.Array U R.DIM2 RGB8)
loadImg imgPath = do
  img <- readImg imgPath
  R.computeUnboxedP $ fromImage img

saveArray :: R.Array D R.DIM2 RGB8 -> IO ()
saveArray img = do
  unboxed <- R.computeUnboxedP img
  (savePngImage outputPath . ImageRGB8 . toImage) unboxed

runCommand :: R.Array U R.DIM2 RGB8 -> String -> [String] -> MaybeT IO (R.Array D R.DIM2 RGB8)
runCommand arr cmd args = do
  let n = R.extent arr
  case map toLower cmd of
    "grayscale" -> mapImage' grayscale
    "only_red" -> mapImage' onlyRed
    "only_green" -> mapImage' onlyGreen
    "only_blue" -> mapImage' onlyBlue
    "negative" -> mapImage' negative
    "only_y" -> mapImage' onlyY
    "only_cb" -> mapImage' onlyCb
    "only_cr" -> mapImage' onlyCr
    "only_h" -> mapImage' onlyH
    "only_l" -> mapImage' onlyL
    "only_s" -> mapImage' onlyS
    "filter_hue" -> mapImage' (twoArgs filterHue)
    "filter_skin" -> mapImage' filterSkin
    "filter_red_eyes" -> mapImage' filterRedEyes
    "average_rgb_filter" -> traverse (averageFilter n)
    "median_rgb_filter" -> traverse (medianFilter n)
    "average_y_filter" -> mapTraverse' (yAverageFilter n) toYcbcr fromYcbcr
    "median_y_filter" -> mapTraverse' (yMedianFilter n) toYcbcr fromYcbcr
    "binarize" -> if length args > 1 then
                     mapImage' (twoArgs binarize)
                  else mapImage' (oneArg (binarize' 0))
    "binarize_otsu" -> liftIO $ Otsu.binarize arr
    "binarize_bernsen" -> liftIO $ Bernsen.binarize arr
    "binarize_mixed" -> liftIO $ oneArg $ Bernsen.mixedBinarize arr
    "erosion" -> liftIO $ Morphology.morphology arr (twoArgs Morphology.erosion n)
    "dilation" -> liftIO $ Morphology.morphology arr (twoArgs Morphology.dilation n)
    "rgb_erosion" -> traverse (Morphology.rgbMorphology (twoArgs Morphology.erosion n))
    "rgb_dilation" -> traverse (Morphology.rgbMorphology (twoArgs Morphology.dilation n))
    "opening" -> liftIO $ Morphology.doubleMorphology arr (twoArgs Morphology.erosion n) (twoArgs Morphology.dilation n)
    "closing" -> liftIO $ Morphology.doubleMorphology arr (twoArgs Morphology.dilation n) (twoArgs Morphology.erosion n)
    "hitandmiss_1" -> hitAndMiss HitAndMiss.hitAndMiss1
    "convex_hull" -> hitAndMiss HitAndMiss.convexHull
    "skeleton" -> hitAndMiss HitAndMiss.skeleton
    "pruning" -> hitAndMiss HitAndMiss.pruning
    _ -> do liftIO $ putStrLn $ "Unknown command: " ++ cmd
            MaybeT $ return Nothing
    where mapImage' f = liftIO $ return $ R.map f arr
          mapTraverse' f map1 map2 = liftIO $ mapTraverse arr f map1 map2
          traverse f = liftIO $ return $ R.traverse arr id f
          oneArg f = f (read $ head args)
          twoArgs f = f (read $ head args) (read $ args !! 1)
          hitAndMiss f = if not (null args) then liftIO $ oneArg $ f arr
                         else liftIO $ f arr 200

mapTraverse :: (RU.Unbox a) => R.Array U R.DIM2 RGB8 ->
               ((R.DIM2 -> a) -> R.DIM2 -> a) ->
               (RGB8 -> a) -> (a -> RGB8) -> IO (R.Array D R.DIM2 RGB8)
mapTraverse arr fun mapFun returnMapFun = do
  mappedArr <- R.computeUnboxedP $ R.map mapFun arr 
  traversedArr <- R.computeUnboxedP (R.traverse mappedArr id fun)
  return $ R.map returnMapFun traversedArr 
