{-# LANGUAGE LambdaCase #-}
-- | Main executable module.
module Main
  ( main,
  )
where

import Control.Monad
import Data.Maybe
import System.Environment
import System.Exit
import System.IO
import Test.Tasty.Config
import Test.Tasty.Discover

-- | Main function.
main :: IO ()
main = do
  name <- getProgName
  getArgs >>= \case
    src : _ : dst : opts ->
      case parseConfig name opts of
        Left err -> do
          hPutStrLn stderr err
          exitFailure
        Right config -> do
          tests <- findTests src config
          let ingredients = tastyIngredients config
              moduleName = fromMaybe "Main" (generatedModuleName config)
              output = generateTestDriver config moduleName ingredients src tests
          when (debug config) $ hPutStrLn stderr output
          writeFile dst output
    _ -> do
      hPutStrLn stderr "Usage: tasty-discover src _ dst [OPTION...]"
      exitFailure
