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
import Data.Char (toLower)
import PixelMaps
import PixelTraversals
import JuicyRepa
import YCbCr (toYcbcr, fromYcbcr, y)
import qualified Otsu as Otsu
import qualified Bernsen as Bernsen
import qualified Criterion.Measurement as Cr

outputPath = "output.png"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "No arguments provided"
    cmd:params -> runCommandHandleExceptions cmd params

runCommandHandleExceptions :: String -> [String] -> IO ()
runCommandHandleExceptions cmd params =
  catch commandRun handler
  where
    commandRun = do
      Cr.initializeTime
      result <- runMaybeT $ runCommand cmd params
      case result of
        Nothing -> putStrLn "Command failed"
        Just () -> do 
          executionTime <- Cr.getTime
          putStr "Command succedded in "
          (putStr . show . round . (*1000)) executionTime
          putStrLn " milliseconds"
          void $ system outputPath
    handler :: SomeException -> IO ()
    handler _ = putStrLn "Arguments don't match. Please check out README."

runCommand :: String -> [String] -> MaybeT IO ()
runCommand cmd args =
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
    "filter_hue" -> mapImage' (filterHue (read $ args !! 1, read $ args !! 2))
    "filter_skin" -> mapImage' filterSkin
    "filter_red_eyes" -> mapImage' filterRedEyes
    "average_rgb_filter" -> traverseImage' averageFilter
    "median_rgb_filter" -> traverseImage' medianFilter
    "average_y_filter" -> traverseMappedImage' yAverageFilter toYcbcr fromYcbcr
    "median_y_filter" -> traverseMappedImage' yMedianFilter toYcbcr fromYcbcr
    "binarize" -> do if length args > 2 then
                       mapImage' (binarize (read $ args !! 1) (read $ args !! 2))
                     else mapImage' (binarize 0 (read $ args !! 1))
    "binarize_otsu" -> otsuBinarize $ head args
    "binarize_bernsen" -> bernsenBinarize $ head args
    _ -> do 
      liftIO $ putStrLn $ "Unknown command: " ++ cmd
      MaybeT $ return Nothing
    where mapImage' f = mapImage f $ head args
          traverseMappedImage' f map returnMap =
            traverseImage f (head args) map returnMap
          traverseImage' f = traverseMappedImage' f id id

bernsenBinarize :: FilePath -> MaybeT IO()
bernsenBinarize imgPath = do
  img <- readImg imgPath
  let yArr = R.map YCbCr.y $ fromImage img
  let dim = R.extent yArr
  computed <- liftIO $ R.computeUnboxedP (Bernsen.binarize dim yArr)
  liftIO $ (savePngImage outputPath . ImageRGB8 . toImage) computed

otsuBinarize :: FilePath -> MaybeT IO()
otsuBinarize imgPath = do
  img <- readImg imgPath
  let arr = fromImage img
  yArr <- R.computeUnboxedP $ R.map YCbCr.y arr
  th <- liftIO $ Otsu.threshold yArr 256
  computed <- liftIO $ R.computeUnboxedP (R.map (binarize 0 th) arr)
  liftIO $ (savePngImage outputPath . ImageRGB8 . toImage) computed

mapImage :: (RGB8 -> RGB8) -> FilePath -> MaybeT IO ()
mapImage fun imgPath = do
  img <- readImg imgPath
  computed <- liftIO $ R.computeUnboxedP (R.map fun (fromImage img))
  liftIO $ (savePngImage outputPath . ImageRGB8 . toImage) computed

traverseImage :: (R.DIM2 -> (R.DIM2 -> a) -> R.DIM2 -> a)
                 -> FilePath -> (RGB8 -> a) -> (a -> RGB8) -> MaybeT IO ()
traverseImage fun imgPath mapFun returnMapFun = do
  img <- readImg imgPath
  let arr = R.map mapFun (fromImage img)
  let fun' = fun $ R.extent arr
  let result = R.map returnMapFun (R.traverse arr id fun')
  computed <- (liftIO . R.computeUnboxedP) result
  liftIO $ (savePngImage outputPath . ImageRGB8 . toImage) computed

readImg :: FilePath -> MaybeT IO (Image PixelRGB8)
readImg path = do
  img <- liftIO $ readImage path
  case img of
    Left err -> do 
      liftIO $ putStrLn $ "Could not read image " ++ path ++ ": " ++ err
      MaybeT $ return Nothing
    Right img -> MaybeT (return $ Just $ convertRGB8 img)
