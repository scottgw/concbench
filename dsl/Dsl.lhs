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

import qualified Statistics.Resampling.Bootstrap as Stats

import           System.Environment()
import qualified System.Clock as Clock

import           Test.QuickCheck (Arbitrary, Gen)
import qualified Test.QuickCheck as QuickCheck

import Bench

data BenchDsl where
    DslVar   :: BenchDsl
    DslFib   :: BenchDsl
    DslCache :: BenchDsl
    DslLock1 :: BenchDsl -> BenchDsl
    DslLock2 :: BenchDsl -> BenchDsl
    DslSeq   :: BenchDsl -> BenchDsl -> BenchDsl
    DslPar   :: BenchDsl -> BenchDsl -> BenchDsl
    deriving (Ord, Eq, Read, Show)

instance Bench BenchDsl where
  genAtom  = return DslFib
  estimate = estimateDsl
  cache    = DslCache
  lock1    = DslLock1
  lock2    = DslLock2
  (|>)     = DslSeq
  (|||)    = DslPar
  benchSize = dslSize
  normalize = canonicalDsl


pretty :: BenchDsl -> String
pretty DslVar = "<x>"
pretty DslFib = "fib"
pretty DslCache = "cache"
pretty (DslLock1 b) = concat ["lock1(", pretty b, ")"]
pretty (DslLock2 b) = concat ["lock2(", pretty b, ")"]
pretty (DslSeq b1 b2) = concat ["(", pretty b1, ") ; (", pretty b2,")"]
pretty (DslPar b1 b2) = concat ["(", pretty b1, ") ||| (", pretty b2 ,")"]

dslSize :: BenchDsl -> Int
dslSize dsl =
    case dsl of
      DslVar       -> 1
      DslFib       -> 1
      DslCache     -> 1
      DslLock1 b   -> 1 + dslSize b
      DslLock2 b   -> 1 + dslSize b
      DslSeq b1 b2 -> 1 + dslSize b1 + dslSize b2
      DslPar b1 b2 -> 1 + dslSize b1 + dslSize b2

hasVar :: BenchDsl -> Bool
hasVar dsl = 
    case dsl of
      DslVar       -> True
      DslLock1 b -> hasVar b
      DslLock2 b -> hasVar b
      DslSeq b1 b2 -> hasVar b1 || hasVar b2
      DslPar b1 b2 -> hasVar b1 || hasVar b2
      _            -> False


instance Arbitrary BenchDsl where
    arbitrary = QuickCheck.sized dslGen
    shrink = canonicalShrink

canonicalShrink :: BenchDsl -> [BenchDsl]
canonicalShrink = map canonicalDsl . shrinkDsl . canonicalDsl

shrinkDsl :: BenchDsl -> [BenchDsl]
shrinkDsl (DslPar a b) = [a, b] ++ combineShrink DslPar a b
shrinkDsl (DslSeq a b) = [a, b] ++ combineShrink DslSeq a b
shrinkDsl (DslLock1 b) = 
  b : QuickCheck.shrink b ++ map DslLock1 (QuickCheck.shrink b)
shrinkDsl (DslLock2 b) = 
  b : QuickCheck.shrink b ++ map DslLock2 (QuickCheck.shrink b)
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

canonicalDsl :: BenchDsl -> BenchDsl
canonicalDsl dsl =
    case dsl of
      DslLock1 b -> DslLock1 (canonicalDsl b)
      DslLock2 b -> DslLock2 (canonicalDsl b)
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

dslGen :: Int -> Gen BenchDsl
dslGen 0 = return DslFib
dslGen 1 = return DslFib
dslGen n = do
  switch :: Int <- QuickCheck.arbitrary
  let op = if switch `rem` 2 == 0 then DslSeq else DslPar 
      l = (n `div` 2) + n `rem` 2
      r = (n `div` 2)
  op <$> dslGen l <*> dslGen r

estimateDsl :: BenchParams BenchDsl
            -> BenchDsl
            -> Stats.Estimate
estimateDsl param ben =
  case ben of
    DslFib -> fibParam param
    DslCache -> cacheParam param
    DslLock1 b -> estim b + lockParam param
    DslLock2 b -> estim b + lockParam param
    DslSeq b1 b2 -> estim b1 + estim b2
    DslPar b1 b2 -> (estim b1 `max` estim b2) + joinParam param
    DslVar -> error "no estimate for DslVar"
  where
    estim = estimateDsl param

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

\begin{code}

\end{code}
