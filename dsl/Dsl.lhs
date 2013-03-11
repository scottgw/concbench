\begin{code}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Dsl where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad

import           Criterion
import           Criterion.Analysis
import           Criterion.Config
import           Criterion.Environment
import           Criterion.Monad

import qualified Statistics.Resampling.Bootstrap as Stats

import           System.Environment()
import qualified System.Clock as Clock

import           Test.QuickCheck (Arbitrary, Gen)
import qualified Test.QuickCheck as QuickCheck

import Bench

data BenchDsl where
    DslFib   :: BenchDsl
    DslLock1 :: Lock -> !BenchDsl -> BenchDsl
    DslLock2 :: Lock -> !BenchDsl -> BenchDsl
    DslSeq   :: !BenchDsl -> !BenchDsl -> BenchDsl
    DslPar   :: !BenchDsl -> !BenchDsl -> BenchDsl

instance Bench BenchDsl where
    genAtom  = return DslFib
    estimate = estimateDsl
    timeActual = measureDsl
    lock1 = DslLock1
    lock2 = DslLock2
    (|>)  = DslSeq
    (|||) = DslPar

instance Show BenchDsl where
    show DslFib = "fib"
    show (DslLock1 _l b) = concat ["lock1(", show b, ")"]
    show (DslLock2 _l b) = concat ["lock2(", show b, ")"]
    show (DslSeq b1 b2) = concat ["(", show b1, ") ; (", show b2,")"]
    show (DslPar b1 b2) = concat ["(", show b1, ") ||| (", show b2,")"]



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

{-# NOINLINE compileBench #-}
compileBench :: BenchDsl -> IO ()
compileBench DslFib  = fibM 32
compileBench (DslLock1 l b) = lock l >> compileBench b >> unlock l
compileBench (DslLock2 l b) = lock l >> compileBench b >> unlock l
compileBench (DslSeq b1 b2) = compileBench b1 >> compileBench b2
compileBench (DslPar b1 b2) = do
  barrier <- newEmptyMVar
  _ <- forkIO (compileBench b1 >> putMVar barrier ())
  _ <- forkIO (compileBench b2 >> putMVar barrier ())
  takeMVar barrier
  takeMVar barrier

{-# NOINLINE timeBench #-}
timeBench :: BenchDsl -> IO Double
timeBench b = fst <$> (timeAction $ compileBench b)



instance Arbitrary BenchDsl where
    arbitrary = QuickCheck.sized dslGen
    shrink = shrinkDsl

shrinkDsl :: BenchDsl -> [BenchDsl]
shrinkDsl (DslPar a b) = [a, b] ++ do
  a' <- QuickCheck.shrink a
  b' <- QuickCheck.shrink b
  return (DslPar a' b')
shrinkDsl (DslSeq a b) =  [a, b] ++ do
  a' <- QuickCheck.shrink a
  b' <- QuickCheck.shrink b
  return (DslSeq a' b')
shrinkDsl (DslLock1 _ b) = [b]
shrinkDsl (DslLock2 _ b) = [b]
shrinkDsl _a           = []

dslGen :: Int -> Gen BenchDsl
dslGen 0 = return DslFib
dslGen 1 = return DslFib
dslGen n = do
  switch :: Int <- QuickCheck.arbitrary
  let op = if switch `rem` 2 == 0 then DslSeq else DslPar 
      l = (n `div` 2) + n `rem` 2
      r = (n `div` 2)
  op <$> dslGen l <*> dslGen r


estimateDsl :: BenchParams BenchDsl -> BenchDsl -> Stats.Estimate
estimateDsl param ben =
  case ben of
    DslFib -> fibParam param
    DslLock1 _l b -> estim b + lockParam param
    DslLock2 _l b -> estim b + lockParam param
    DslSeq b1 b2 -> estim b1 + estim b2
    DslPar b1 b2 -> (estim b1 `max` estim b2) + joinParam param
  where
    estim = estimateDsl param

measureDsl :: Environment -> BenchDsl -> IO Stats.Estimate
measureDsl env b = do
  sample <- sampleM
  anMean <$> analyseSample 0.95 sample 20
 where 
   sampleM = withConfig defaultConfig $ 
             runBenchmark env (compileBench b)



timeAction :: IO a -> IO (Double, a)
timeAction act = do
  t1 <- Clock.getTime Clock.Monotonic -- getCurrentTime
  !r <- act
  t2 <- Clock.getTime Clock.Monotonic -- getCurrentTime
  let secDiff = Clock.sec t2 - Clock.sec t1
      nanoDiff = Clock.nsec t2 - Clock.nsec t1
      diff = fromIntegral secDiff + (fromIntegral nanoDiff / 10^(9::Int))
  return (diff, r)


instance Num Stats.Estimate where
  Stats.Estimate av1 lower1 upper1 _ -
    Stats.Estimate av2 lower2 upper2 confLevel2 =
      Stats.Estimate (av1 - av2) (lower1 - lower2) (upper1 - upper2) confLevel2
  Stats.Estimate av1 lower1 upper1 _ +
    Stats.Estimate av2 lower2 upper2 confLevel2 =
      Stats.Estimate (av1 + av2) (lower1 + lower2) (upper1 + upper2) confLevel2


instance Ord Stats.Estimate where
  compare e1 e2 = compare (Stats.estPoint e1) (Stats.estPoint e2)


\end{code}
