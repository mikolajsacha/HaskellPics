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
import PixelMaps
import PixelTraversals
import JuicyRepa
import YCbCr (toYcbcr, fromYcbcr, y)
import qualified Otsu
import qualified Bernsen
import Morphology 
import qualified Criterion.Measurement as Cr

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

runCommand :: R.Array U R.DIM2 RGB8 -> String -> [String] -> MaybeT IO (R.Array U R.DIM2 RGB8)
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
    "average_rgb_filter" -> traverseImage' (averageFilter n)
    "median_rgb_filter" -> traverseImage' (medianFilter n)
    "average_y_filter" -> traverseMappedImage' (yAverageFilter n) toYcbcr fromYcbcr
    "median_y_filter" -> traverseMappedImage' (yMedianFilter n) toYcbcr fromYcbcr
    "binarize" -> if length args > 2 then
                     mapImage' (twoArgs binarize)
                  else mapImage' (oneArg (binarize' 0))
    "binarize_otsu" -> liftIO $ otsuBinarize arr
    "binarize_bernsen" -> liftIO $ bernsenBinarize arr
    "binarize_mixed" -> liftIO $ oneArg $ mixedBinarize arr
    "erosion" -> liftIO $ morphology arr (twoArgs erosion n)
    "dilation" -> liftIO $ morphology arr (twoArgs dilation n)
    "rgb_erosion" -> traverseImage' (rgbMorphology (twoArgs erosion n))
    "rgb_dilation" -> traverseImage' (rgbMorphology (twoArgs dilation n))
    "opening" -> liftIO $ doubleMorphology arr (twoArgs erosion n) (twoArgs dilation n)
    "closing" -> liftIO $ doubleMorphology arr (twoArgs dilation n) (twoArgs erosion n)
    _ -> do liftIO $ putStrLn $ "Unknown command: " ++ cmd
            MaybeT $ return Nothing
    where mapImage' f = liftIO $ mapImage arr f
          traverseMappedImage' f map1 map2 = liftIO $ traverseImage arr f map1 map2
          traverseImage' f = traverseMappedImage' f id id
          oneArg f = f (read $ head args)
          twoArgs f = f (read $ head args) (read $ args !! 1)

saveArray :: R.Array U R.DIM2 RGB8 -> IO ()
saveArray = savePngImage outputPath . ImageRGB8 . toImage

mapImage :: (RU.Unbox a, RU.Unbox b) => R.Array U R.DIM2 a -> (a -> b) -> IO (R.Array U R.DIM2 b)
mapImage arr fun = R.computeUnboxedP (R.map fun arr)

traverseImage :: (RU.Unbox a) => R.Array U R.DIM2 RGB8 ->
                 ((R.DIM2 -> a) -> R.DIM2 -> a) ->
                 (RGB8 -> a) -> (a -> RGB8) -> IO (R.Array U R.DIM2 RGB8)
traverseImage arr fun mapFun returnMapFun = do
  mappedArr <- mapImage arr mapFun
  traversedArr <- R.computeUnboxedP (R.traverse mappedArr id fun)
  mapImage traversedArr returnMapFun

otsuThreshold :: R.Array U R.DIM2 RGB8 -> IO Double
otsuThreshold arr = do
  yArr <- R.computeUnboxedP $ R.map YCbCr.y arr
  Otsu.threshold yArr 256

mixedBinarize :: R.Array U R.DIM2 RGB8 -> Double -> IO (R.Array U R.DIM2 RGB8)
mixedBinarize arr threshold = do
  yArr <- R.computeUnboxedP $ R.map YCbCr.y arr
  Bernsen.mixedBinarize yArr threshold

bernsenBinarize :: R.Array U R.DIM2 RGB8 -> IO (R.Array U R.DIM2 RGB8)
bernsenBinarize arr = do
  yArr <- R.computeUnboxedP $ R.map YCbCr.y arr
  R.computeUnboxedP (Bernsen.binarize yArr)

otsuBinarize :: R.Array U R.DIM2 RGB8 -> IO (R.Array U R.DIM2 RGB8)
otsuBinarize arr = do
  th <- otsuThreshold arr
  R.computeUnboxedP (R.map (binarize' 0 th) arr)

morphology :: R.Array U R.DIM2 RGB8 ->
              ((R.DIM2 -> Bool) -> R.DIM2 -> Bool) ->
              IO (R.Array U R.DIM2 RGB8)
morphology arr fun = doubleMorphology arr fun id

doubleMorphology :: R.Array U R.DIM2 RGB8 ->
                    ((R.DIM2 -> Bool) -> R.DIM2 -> Bool) -> 
                    ((R.DIM2 -> Bool) -> R.DIM2 -> Bool) ->
                    IO (R.Array U R.DIM2 RGB8)
doubleMorphology arr fun1 fun2 = do
  th <- otsuThreshold arr
  binarized <- R.computeUnboxedP $ R.map (binarize 0 th) arr
  afterMorph1 <- R.computeUnboxedP $ R.traverse binarized id fun1
  afterMorph2 <- R.computeUnboxedP $ R.traverse afterMorph1 id fun2
  R.computeUnboxedP $ R.map (triple . boundedToBounded) afterMorph2

