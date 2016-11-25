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
import JuicyRepa (RGB8, fromImage, toImage)

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
    "grayscale" -> runImageMap grayscale $ head args
    _ -> do 
      liftIO $ putStrLn $ "Unknown command: " ++ cmd
      MaybeT $ return Nothing

runImageMap :: ((R.DIM2 -> RGB8) -> R.DIM2 -> RGB8) -> FilePath -> MaybeT IO ()
runImageMap fun imgPath = do
  img <- readImg imgPath
  computed <- liftIO $ R.computeUnboxedP (R.traverse (fromImage img) id fun)
  liftIO $ (savePngImage outputPath . ImageRGB8 . toImage) computed

readImg :: FilePath -> MaybeT IO (Image PixelRGB8)
readImg path = do
  img <- liftIO $ readImage path
  case img of
    Left err -> do 
      liftIO $ putStrLn $ "Could not read image " ++ path ++ ": " ++ err
      MaybeT $ return Nothing
    Right img -> MaybeT (return $ Just $ convertRGB8 img)
