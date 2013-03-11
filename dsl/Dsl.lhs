\begin{code}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    arbitrary = QuickCheck.sized benchGen
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

data BenchSel = BenchSelPar | BenchSelSeq | BenchSelLock1| BenchSelLock2 
              deriving (Bounded, Enum)


benchGen :: Int -> Gen BenchDsl
benchGen 0 = return DslFib
benchGen 1 = return DslFib
benchGen n = do
  switch :: Int <- QuickCheck.arbitrary
  let op = if switch `rem` 2 == 0 then DslSeq else DslPar 
      l = (n `div` 2) + n `rem` 2
      r = (n `div` 2)
  op <$> benchGen l <*> benchGen r

benchGenPar :: Maybe Lock -> Maybe Lock -> Int -> Int -> Gen BenchDsl
benchGenPar lk1 lk2 parLimit n = snd <$> benchGenPar' lk1 lk2 parLimit n

benchGenPar' :: Maybe Lock -> Maybe Lock -> Int -> Int -> Gen (Int, BenchDsl)
benchGenPar' _ _ _ 0 = return (0, DslFib)
benchGenPar' _ _ _ 1 = return (0, DslFib)
benchGenPar' lk1Mb lk2Mb parLimit n = do
  sel <- QuickCheck.arbitraryBoundedEnum
  let lSize = (n `div` 2) + n `rem` 2
      rSize = (n `div` 2)
  case sel of
    BenchSelPar -> 
        if parLimit == 0
        then benchGenPar' lk1Mb lk2Mb parLimit n
        else do
          let parLimit' = parLimit - 1
          (lNumPar, l) <- benchGenPar' lk1Mb lk2Mb parLimit' lSize
          let parLimit'' = parLimit' - lNumPar
          (rNumPar, r) <- benchGenPar' lk1Mb lk2Mb parLimit'' rSize
          return (lNumPar + rNumPar + 1,  DslPar l r)
    BenchSelSeq -> do
             (lPar, b1) <- benchGenPar' lk1Mb lk2Mb parLimit lSize
             (rPar, b2) <- benchGenPar' lk1Mb lk2Mb (parLimit - lPar) lSize
             return (lPar + rPar, DslSeq b1 b2)
    BenchSelLock1 -> 
        case lk1Mb of
          Just lk1 -> 
              do
                (p, b) <- benchGenPar' Nothing lk2Mb parLimit (n-1)
                return (p, DslLock1 lk1 b)
          Nothing -> benchGenPar' lk1Mb lk2Mb parLimit n
    BenchSelLock2 -> 
        case lk2Mb of
          Just lk2 -> 
              do
                (p, b) <- benchGenPar' lk1Mb Nothing parLimit (n-1)
                return (p, DslLock2 lk2 b)
          Nothing -> benchGenPar' lk1Mb lk2Mb parLimit n


estimateDsl :: BenchParams -> BenchDsl -> Stats.Estimate
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
