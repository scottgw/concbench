{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [numWorkersStr] -> noShareTest (read numWorkersStr)
    _ -> putStrLn "Expecting only 2 arguments"

-- No share test
noShareTest :: Int -> IO ()
noShareTest numWorkers = void $ mapConcurrently id fibs
  where
    fibIO = atomically (return $! fib 40)    
    fibs = replicate numWorkers fibIO

fib :: Int -> Int
fib n
  | n < 2 = 1
  | otherwise = fib (n-1) + fib (n-2)
