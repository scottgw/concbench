{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

import qualified Data.Map as Map
import Data.Array.MArray

import System.Environment

-- Share test
shareTest :: TArray Int Int -> IO ()
shareTest array = do
  (lower, upper) <- atomically $ getBounds array
  let
    n = 32
    
    modifyArray i = do
      !x <- readArray array i
      let !x' = x + 1
      writeArray array i x'
    
    arrayWorker = mapM_ modifyArray [lower .. upper]
    
    arrayWorkers = replicate n (atomically arrayWorker)
  void $ mapConcurrently id arrayWorkers
    
-- Barrier test
barrierTest :: TVar Int -> TVar Int -> IO ()
barrierTest doneVar var = void $ mapConcurrently id barrierWorkers
  where
    reps = 2000
    n = 4
    
    joinBarrier = do
      x <- readTVar var
      check (x /= n)
      writeTVar var (x+1)
    
    waitBarrier = do
      x <- readTVar var
      check (x == n)
      modifyTVar'  doneVar (+1)
      done <- readTVar doneVar
      if done == n
        then writeTVar var 0 >> writeTVar doneVar 0
        else return ()
    
    barrierWork = atomically joinBarrier >> atomically waitBarrier
      
    repeatJoin = replicateM_ reps barrierWork
    barrierWorkers = replicate n repeatJoin

-- No share test
noShareTest :: IO ()
noShareTest = void $ mapConcurrently id fibs
  where
    numWorkers = 4
    
    fibIO = atomically (return $! fib 40)
    
    fibs = replicate numWorkers fibIO

fib :: Int -> Int
fib n
  | n < 2 = 1
  | otherwise = fib (n-1) + fib (n-2)


-- N-M producer consumer test
prodConsTest :: TQueue () -> IO ()
prodConsTest conduit =
  let prods = replicate numProds (atomically producer)
      cons = replicate numCons (atomically consumer)
      
      consumer = replicateM_ perCons (readTQueue conduit)
      producer = replicateM_ perProd (writeTQueue conduit ())

      numProds = 32 
      numCons = 32
      perProd = 20000 
      perCons = 20000

  in void $ mapConcurrently id (prods ++ cons)

-- Mutex test
mutexTest :: TVar Int -> IO ()
mutexTest var = void $ mapConcurrently id muts >> (atomically (readTVar var) >>= print)
  where
    numActors = 32
    reps = 200000
    
    muts = replicate numActors mutexLoop
    
    mutexIteration = do
      !x <- readTVar var
      let !x' = x + 1
      writeTVar var x'

    mutexLoop = atomically $ replicateM_ reps mutexIteration


-- Condition test
conditionTest :: TVar Int -> IO ()
conditionTest var =
  void $ mapConcurrently id (prods ++ cons)
  where
    numProds = 32
    numCons = 32
    perProd = 5000
    perCons = 5000
    
    consumerAction = do
      !x <- readTVar var
      check (x `rem` 2 == 0)
      let !x' = x + 1
      writeTVar var x'
    
    producerAction = do
      !x <- readTVar var
      check (x `rem` 2 == 1)
      let !x' = x + 1
      writeTVar var x'

    producerActions = replicateM_ perProd (atomically producerAction)
    consumerActions = replicateM_ perCons (atomically consumerAction)

    prods = replicate numProds producerActions
    cons = replicate numCons consumerActions

-- Testing setup
tests :: Map.Map String (IO ())
tests = Map.fromList [("mutex", newTVarIO 0 >>= mutexTest)
                     ,("n-m", newTQueueIO >>= prodConsTest)
                     ,("cond", newTVarIO 0 >>= conditionTest)
                     ,("noshare", noShareTest)
                     ,("barrier", do
                          doneVar <- newTVarIO 0
                          var <- newTVarIO 0
                          barrierTest doneVar var)
                     ,("share", atomically (newArray (0,5000-1) 0) >>= 
                                shareTest)
                     ]

testNames :: String
testNames = show $ Map.keys tests

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn $ "No arguments given, please pick one of " ++ testNames
    testName:_rest -> case Map.lookup testName tests of
      Nothing -> putStrLn $
                 concat ["test: ", testName, " not found in " , testNames]
      Just testAction -> testAction