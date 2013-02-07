{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

import Data.Array.MArray

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [numWorkersStr, numElemsStr] -> do
      array <- atomically (newArray (0, read numElemsStr - 1) 0)
      shareTest (read numWorkersStr) array
    _ -> putStrLn "Expecting only 2 arguments"



-- Share test
shareTest :: Int -> TArray Int Int -> IO ()
shareTest numWorkers array = do
  (lower, upper) <- atomically $ getBounds array
  let
    modifyArray i = do
      !x <- readArray array i
      let !x' = x + 1
      writeArray array i x'
    
    arrayWorker = mapM_ modifyArray [lower .. upper]
    
    arrayWorkers = replicate numWorkers (atomically arrayWorker)
  void $ mapConcurrently id arrayWorkers
