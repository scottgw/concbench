{-# LANGUAGE BangPatterns #-}
module EmbedDsl where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad

import           Criterion
import           Criterion.Analysis
import           Criterion.Config
import           Criterion.Environment
import           Criterion.Monad

import qualified Statistics.Resampling.Bootstrap as Stats

import Bench
import Cache
import Dsl

fib :: Int -> Int
fib n | n > 1     = fib (n-1) + fib (n-2)
      | otherwise = 1

{-# NOINLINE fibM #-}
fibM :: Monad m => Int -> m ()
fibM n =
  let !_x = fib n
  in return ()

lock    :: Lock -> IO ()
lock    = void . takeMVar

unlock  :: Lock -> IO ()
unlock  = void . flip putMVar ()  

type Lock = MVar ()

{-# NOINLINE compileBench #-}
compileBench :: BenchDsl -> IO ()
compileBench DslFib  = fibM 32
compileBench (DslCache mem) = memTask mem
compileBench (DslLock1 b) = lock l >> compileBench b >> unlock l
compileBench (DslLock2 b) = lock l >> compileBench b >> unlock l
compileBench (DslSeq b1 b2) = compileBench b1 >> compileBench b2
compileBench (DslPar b1 b2) = do
  barrier <- newEmptyMVar
  _ <- forkIO (compileBench b1 >> putMVar barrier ())
  _ <- forkIO (compileBench b2 >> putMVar barrier ())
  takeMVar barrier
  takeMVar barrier


{-# NOINLINE timeBench #-}
timeBench :: BenchDsl Lock Memory -> IO Double
timeBench b = fst <$> (timeAction $ compileBench b)

measureDsl :: Environment -> BenchDsl Lock Memory -> IO Stats.Estimate
measureDsl env b = do
  sample <- sampleM
  anMean <$> analyseSample 0.95 sample 20
 where 
   sampleM = withConfig defaultConfig $ 
             runBenchmark env (compileBench b)


