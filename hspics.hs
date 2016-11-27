{-# LANGUAGE TypeOperators   #-}

module Main (main) where

import Codec.Picture
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import qualified Codec.Picture.Types as M
import qualified Data.Array.Repa as R
import Data.Array.Repa (U, D, Z (..), (:.)(..))
import Data.Char (toLower)
import PixelMaps
import PixelTraversals
import JuicyRepa

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
      result <- runMaybeT $ runCommand cmd params
      case result of
        Nothing -> putStrLn "Command failed"
        Just () -> putStrLn "Command succedded"
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
    _ -> do 
      liftIO $ putStrLn $ "Unknown command: " ++ cmd
      MaybeT $ return Nothing
    where mapImage' f = mapImage f $ head args
          traverseImage' f = traverseImage f $ head args

mapImage :: (RGB8 -> RGB8) -> FilePath -> MaybeT IO ()
mapImage fun imgPath = do
  img <- readImg imgPath
  computed <- liftIO $ R.computeUnboxedP (R.map fun (fromImage img))
  liftIO $ (savePngImage outputPath . ImageRGB8 . toImage) computed

traverseImage :: (R.DIM2 -> (R.DIM2 -> RGB8) -> R.DIM2 -> RGB8)
              -> FilePath -> MaybeT IO ()
traverseImage fun imgPath = do
  img <- readImg imgPath
  let arr = fromImage img
  let fun' = fun $ R.extent arr
  computed <- liftIO $ R.computeUnboxedP (R.traverse arr id fun')
  liftIO $ (savePngImage outputPath . ImageRGB8 . toImage) computed

readImg :: FilePath -> MaybeT IO (Image PixelRGB8)
readImg path = do
  img <- liftIO $ readImage path
  case img of
    Left err -> do 
      liftIO $ putStrLn $ "Could not read image " ++ path ++ ": " ++ err
      MaybeT $ return Nothing
    Right img -> MaybeT (return $ Just $ convertRGB8 img)
