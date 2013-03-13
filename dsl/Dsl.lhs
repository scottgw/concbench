\begin{code}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
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

data BenchDsl lock where
    DslFib   :: BenchDsl lock
    DslLock1 :: lock -> BenchDsl lock -> BenchDsl lock
    DslLock2 :: lock -> BenchDsl lock -> BenchDsl lock
    DslSeq   :: BenchDsl lock -> BenchDsl lock -> BenchDsl lock
    DslPar   :: BenchDsl lock -> BenchDsl lock -> BenchDsl lock
    deriving (Ord, Eq)

instance Bench (BenchDsl lock) lock where
    genAtom  = return DslFib
    estimate = estimateDsl
    lock1    = DslLock1
    lock2    = DslLock2
    (|>)     = DslSeq
    (|||)    = DslPar
    benchSize = dslSize
    normalize = canonicalDsl

instance Show (BenchDsl lock) where
    show DslFib = "fib"
    show (DslLock1 _l b) = concat ["lock1(", show b, ")"]
    show (DslLock2 _l b) = concat ["lock2(", show b, ")"]
    show (DslSeq b1 b2) = concat ["(", show b1, ") ; (", show b2,")"]
    show (DslPar b1 b2) = concat ["(", show b1, ") ||| (", show b2,")"]

dslSize dsl =
    case dsl of
      DslFib         -> 1
      DslLock1 _lk b -> 1 + dslSize b
      DslLock2 _lk b -> 1 + dslSize b
      DslSeq b1 b2   -> 1 + dslSize b1 + dslSize b2
      DslPar b1 b2   -> 1 + dslSize b1 + dslSize b2

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
compileBench :: BenchDsl Lock -> IO ()
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
timeBench :: BenchDsl Lock -> IO Double
timeBench b = fst <$> (timeAction $ compileBench b)



instance Arbitrary (BenchDsl lock) where
    arbitrary = QuickCheck.sized dslGen
    shrink = canonicalShrink

canonicalShrink = map canonicalDsl . shrinkDsl . canonicalDsl

shrinkDsl :: BenchDsl lock -> [BenchDsl lock]
shrinkDsl (DslPar a b) = [a, b] ++ combineShrink DslPar a b
shrinkDsl (DslSeq a b) =  [a, b] ++ combineShrink DslSeq a b
shrinkDsl (DslLock1 lk b) = b : QuickCheck.shrink b ++ map (DslLock1 lk) (QuickCheck.shrink b)
shrinkDsl (DslLock2 lk b) = b : QuickCheck.shrink b ++ map (DslLock2 lk) (QuickCheck.shrink b)
shrinkDsl _a           = []

combineShrink c a b = 
    concat [ shrinkWith shrinka shrinkb
           , shrinkWith (a:shrinka) shrinkb
           , shrinkWith shrinka (b:shrinkb)
           , shrinka
           , shrinkb
           ]
    where shrinkWith as bs  = 
              do a' <- as
                 b' <- bs
                 return (c a' b')
          shrinka = QuickCheck.shrink a
          shrinkb = QuickCheck.shrink b

canonicalDsl :: BenchDsl lock -> BenchDsl lock
canonicalDsl dsl =
    case dsl of
      DslFib -> DslFib
      DslLock1 lk1 b -> DslLock1 lk1 (canonicalDsl b)
      DslLock2 lk2 b -> DslLock2 lk2 (canonicalDsl b)
      DslSeq b1 b2   -> shiftSeq (DslSeq b1 b2)
      DslPar b1 b2   -> shiftPar (DslPar b1 b2)

    where
      shiftSeq (DslSeq (DslSeq b1 b2) b3) = shiftSeq (DslSeq b1 (DslSeq b2 b3))
      shiftSeq (DslSeq b1 b2)             = DslSeq b1 (shiftSeq b2)
      shiftSeq b                          = b

      shiftPar (DslPar (DslPar b1 b2) b3) = shiftPar (DslPar b1 (DslPar b2 b3))
      shiftPar (DslPar b1 b2)             = DslPar b1 (shiftPar b2)
      shiftPar b                          = b

dslGen :: Int -> Gen (BenchDsl lock)
dslGen 0 = return DslFib
dslGen 1 = return DslFib
dslGen n = do
  switch :: Int <- QuickCheck.arbitrary
  let op = if switch `rem` 2 == 0 then DslSeq else DslPar 
      l = (n `div` 2) + n `rem` 2
      r = (n `div` 2)
  op <$> dslGen l <*> dslGen r


estimateDsl :: BenchParams (BenchDsl lock)
            -> BenchDsl lock
            -> Stats.Estimate
estimateDsl param ben =
  case ben of
    DslFib -> fibParam param
    DslLock1 _l b -> estim b + lockParam param
    DslLock2 _l b -> estim b + lockParam param
    DslSeq b1 b2 -> estim b1 + estim b2
    DslPar b1 b2 -> (estim b1 `max` estim b2) + joinParam param
  where
    estim = estimateDsl param

measureDsl :: Environment -> BenchDsl Lock -> IO Stats.Estimate
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
