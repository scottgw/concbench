\begin{code}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Bench where

import           Control.Applicative

import           Criterion.Environment

import           Test.QuickCheck (Arbitrary, Gen)
import qualified Test.QuickCheck as QuickCheck

import qualified Statistics.Resampling.Bootstrap as Stats


class RunnableBench a where
    timeActual :: Environment -> a -> IO Stats.Estimate
--    timeWithHold :: 

class Arbitrary a => Bench a where
    type Env a
    genAtom  :: Gen a
    estimate :: BenchParams -> a -> Stats.Estimate
    cache    :: a
    sleep    :: a
    lock1    :: a -> a
    lock2    :: a -> a
    (|>)     :: a -> a -> a
    (|||)    :: a -> a -> a
    benchSize :: a -> Int
    normalize :: a -> a

data BenchParams =
  BenchParams 
  { fibParam :: Stats.Estimate
  , cacheParam :: Stats.Estimate
  , sleepParam :: Stats.Estimate
  , lockParam :: Stats.Estimate
  , joinParam :: Stats.Estimate
  }

instance Show BenchParams where
    show params = concat ["fib: ", show $ fibParam params
                         ,"lock: ", show $ lockParam params
                         ,"join: ", show $ joinParam params
                         ]

data BenchSel = 
  BenchSelPar | BenchSelSeq | BenchSelLock1| BenchSelLock2 
  deriving (Bounded, Enum)

benchGen :: Bench a => Int -> Int -> Gen a
benchGen parLimit n = 
  benchGenAnd (map return [cache, sleep]) parLimit n

benchGenAnd :: Bench b => [Gen b] -> Int -> Int -> Gen b
benchGenAnd others parLimit n = 
  snd <$> benchGenAnd' others parLimit n
benchGenAnd' :: Bench a
             => [Gen a]
             -> Int 
             -> Int 
             -> Gen (Int, a)
benchGenAnd' others _ n
  | n <= 1 = (0,) <$> QuickCheck.oneof (genAtom:others)
benchGenAnd' others parLimit n = do
  sel <- QuickCheck.arbitraryBoundedEnum
  let lSize = (n `div` 2) + n `rem` 2
      rSize = (n `div` 2)
  case sel of
    BenchSelPar -> 
        if parLimit == 0
        then benchGenAnd' others parLimit n
        else do
          let parLimit' = parLimit - 1
          (lNumPar, l) <- benchGenAnd' others parLimit' lSize
          let parLimit'' = parLimit' - lNumPar
          (rNumPar, r) <- benchGenAnd' others parLimit'' rSize
          return (lNumPar + rNumPar + 1,  l ||| r)
    BenchSelSeq -> do
             (lPar, b1) <- benchGenAnd' others parLimit lSize
             (rPar, b2) <- benchGenAnd' others (parLimit - lPar) lSize
             return (lPar + rPar, b1 |> b2)
    BenchSelLock1 -> 
      do (p, b) <- benchGenAnd' others parLimit (n-1)
         return (p, lock1 b)
    BenchSelLock2 -> 
      do (p, b) <- benchGenAnd' others  parLimit (n-1)
         return (p, lock2 b)

\end{code}
