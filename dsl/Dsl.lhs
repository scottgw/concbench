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
import Cache

data BenchDsl lock mem where
    DslVar   :: BenchDsl lock mem
    DslFib   :: BenchDsl lock mem
    DslCache :: mem  -> BenchDsl lock mem
    DslLock1 :: lock -> BenchDsl lock mem -> BenchDsl lock mem
    DslLock2 :: lock -> BenchDsl lock mem -> BenchDsl lock mem
    DslSeq   :: BenchDsl lock mem -> BenchDsl lock mem -> BenchDsl lock mem
    DslPar   :: BenchDsl lock mem -> BenchDsl lock mem -> BenchDsl lock mem
    deriving (Ord, Eq, Read, Show)

instance Bench (BenchDsl lock mem) lock mem where
    genAtom  = return DslFib
    estimate = estimateDsl
    cache    = DslCache
    lock1    = DslLock1
    lock2    = DslLock2
    (|>)     = DslSeq
    (|||)    = DslPar
    benchSize = dslSize
    normalize = canonicalDsl


pretty :: BenchDsl lock mem -> String
pretty DslVar = "<var>"
pretty DslFib = "fib"
pretty (DslCache _) = "cache"
pretty (DslLock1 _l b) = concat ["lock1(", pretty b, ")"]
pretty (DslLock2 _l b) = concat ["lock2(", pretty b, ")"]
pretty (DslSeq b1 b2) = concat ["(", pretty b1, ") ; (", pretty b2,")"]
pretty (DslPar b1 b2) = concat ["(", pretty b1, ") ||| (", pretty b2 ,")"]

dslSize :: BenchDsl lock mem -> Int
dslSize dsl =
    case dsl of
      DslVar         -> 1
      DslFib         -> 1
      DslCache _     -> 1
      DslLock1 _lk b -> 1 + dslSize b
      DslLock2 _lk b -> 1 + dslSize b
      DslSeq b1 b2   -> 1 + dslSize b1 + dslSize b2
      DslPar b1 b2   -> 1 + dslSize b1 + dslSize b2

hasVar :: BenchDsl lock mem -> Bool
hasVar dsl = 
    case dsl of
      DslVar       -> True
      DslLock1 _ b -> hasVar b
      DslLock2 _ b -> hasVar b
      DslSeq b1 b2 -> hasVar b1 || hasVar b2
      DslPar b1 b2 -> hasVar b1 || hasVar b2
      _            -> False

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
compileBench :: BenchDsl Lock Memory -> IO ()
compileBench DslFib  = fibM 32
compileBench (DslCache mem) = memTask mem
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
timeBench :: BenchDsl Lock Memory -> IO Double
timeBench b = fst <$> (timeAction $ compileBench b)

instance Arbitrary (BenchDsl lock mem) where
    arbitrary = QuickCheck.sized dslGen
    shrink = canonicalShrink

canonicalShrink :: BenchDsl lock mem -> [BenchDsl lock mem]
canonicalShrink = map canonicalDsl . shrinkDsl . canonicalDsl

shrinkDsl :: BenchDsl lock mem -> [BenchDsl lock mem]
shrinkDsl (DslPar a b) = [a, b] ++ combineShrink DslPar a b
shrinkDsl (DslSeq a b) = [a, b] ++ combineShrink DslSeq a b
shrinkDsl (DslLock1 lk b) = 
  b : QuickCheck.shrink b ++ map (DslLock1 lk) (QuickCheck.shrink b)
shrinkDsl (DslLock2 lk b) = 
  b : QuickCheck.shrink b ++ map (DslLock2 lk) (QuickCheck.shrink b)
shrinkDsl _a           = []

combineShrink :: Arbitrary a => (a -> a -> a) -> a -> a -> [a]
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

canonicalDsl :: BenchDsl lock mem -> BenchDsl lock mem
canonicalDsl dsl =
    case dsl of
      DslLock1 lk1 b -> DslLock1 lk1 (canonicalDsl b)
      DslLock2 lk2 b -> DslLock2 lk2 (canonicalDsl b)
      DslSeq b1 b2   -> shiftSeq (DslSeq b1 b2)
      DslPar b1 b2   -> shiftPar (DslPar b1 b2)
      b              -> b

    where
      shiftSeq (DslSeq (DslSeq b1 b2) b3) = shiftSeq (DslSeq b1 (DslSeq b2 b3))
      shiftSeq (DslSeq b1 b2)             = DslSeq b1 (shiftSeq b2)
      shiftSeq b                          = b

      shiftPar (DslPar (DslPar b1 b2) b3) = shiftPar (DslPar b1 (DslPar b2 b3))
      shiftPar (DslPar b1 b2)             = DslPar b1 (shiftPar b2)
      shiftPar b                          = b

dslGen :: Int -> Gen (BenchDsl lock mem)
dslGen 0 = return DslFib
dslGen 1 = return DslFib
dslGen n = do
  switch :: Int <- QuickCheck.arbitrary
  let op = if switch `rem` 2 == 0 then DslSeq else DslPar 
      l = (n `div` 2) + n `rem` 2
      r = (n `div` 2)
  op <$> dslGen l <*> dslGen r

estimateDsl :: BenchParams (BenchDsl lock mem)
            -> BenchDsl lock mem
            -> Stats.Estimate
estimateDsl param ben =
  case ben of
    DslFib -> fibParam param
    DslCache _ -> cacheParam param
    DslLock1 _l b -> estim b + lockParam param
    DslLock2 _l b -> estim b + lockParam param
    DslSeq b1 b2 -> estim b1 + estim b2
    DslPar b1 b2 -> (estim b1 `max` estim b2) + joinParam param
  where
    estim = estimateDsl param

measureDsl :: Environment -> BenchDsl Lock Memory -> IO Stats.Estimate
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
  fromInteger i = 
    Stats.Estimate (fromIntegral i) (fromIntegral i) (fromIntegral i) 0
  signum = error "Estimate doesn't have 'signum' definition"
  (*) = error "Estimate doesn't have '*' definition"
  abs (Stats.Estimate av lower upper conf) = 
    Stats.Estimate (abs av) (abs lower) (abs upper) conf

instance Ord Stats.Estimate where
  compare e1 e2 = compare (Stats.estPoint e1) (Stats.estPoint e2)


\end{code}
