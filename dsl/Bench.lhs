\begin{code}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Bench where

import           Control.Applicative
import           Control.Concurrent

import           Criterion.Environment

import qualified Data.Vector.Unboxed.Mutable as UVM

import           Test.QuickCheck (Arbitrary, Gen)
import qualified Test.QuickCheck as QuickCheck

import qualified Statistics.Resampling.Bootstrap as Stats


class RunnableBench a where
    timeActual :: Environment -> a -> IO Stats.Estimate

class (Show a, Arbitrary a) => Bench a lock | a -> lock where
    genAtom  :: Gen a
    estimate :: BenchParams a -> a -> Stats.Estimate
    lock1    :: lock -> a -> a
    lock2    :: lock -> a -> a
    (|>)     :: a -> a -> a
    (|||)    :: a -> a -> a
    benchSize :: a -> Int
    normalize :: a -> a

data BenchParams a =
  BenchParams 
  { fibParam :: Stats.Estimate
  , lockParam :: Stats.Estimate
  , joinParam :: Stats.Estimate
  }

instance Show (BenchParams a) where
    show params = concat ["fib: ", show $ fibParam params
                         ,"lock: ", show $ lockParam params
                         ,"join: ", show $ joinParam params
                         ]

type Lock = MVar ()
type Memory = UVM.IOVector Int


data BenchSel = BenchSelPar | BenchSelSeq | BenchSelLock1| BenchSelLock2 
              deriving (Bounded, Enum)

benchGen :: Bench a lock => Maybe lock -> Maybe lock -> Int -> Int -> Gen a
benchGen lk1 lk2 parLimit n = snd <$> benchGen' lk1 lk2 parLimit n

benchGen' :: Bench a lock => Maybe lock -> Maybe lock -> Int -> Int -> Gen (Int, a)
benchGen' _ _ _ 0 = (0,) <$> genAtom
benchGen' _ _ _ 1 = (0,) <$> genAtom
benchGen' lk1Mb lk2Mb parLimit n = do
  sel <- QuickCheck.arbitraryBoundedEnum
  let lSize = (n `div` 2) + n `rem` 2
      rSize = (n `div` 2)
  case sel of
    BenchSelPar -> 
        if parLimit == 0
        then benchGen' lk1Mb lk2Mb parLimit n
        else do
          let parLimit' = parLimit - 1
          (lNumPar, l) <- benchGen' lk1Mb lk2Mb parLimit' lSize
          let parLimit'' = parLimit' - lNumPar
          (rNumPar, r) <- benchGen' lk1Mb lk2Mb parLimit'' rSize
          return (lNumPar + rNumPar + 1,  l ||| r)
    BenchSelSeq -> do
             (lPar, b1) <- benchGen' lk1Mb lk2Mb parLimit lSize
             (rPar, b2) <- benchGen' lk1Mb lk2Mb (parLimit - lPar) lSize
             return (lPar + rPar, b1 |> b2)
    BenchSelLock1 -> 
        case lk1Mb of
          Just lk1 -> 
              do
                (p, b) <- benchGen' Nothing lk2Mb parLimit (n-1)
                return (p, lock1 lk1 b)
          Nothing -> benchGen' lk1Mb lk2Mb parLimit n
    BenchSelLock2 -> 
        case lk2Mb of
          Just lk2 -> 
              do
                (p, b) <- benchGen' lk1Mb Nothing parLimit (n-1)
                return (p, lock2 lk2 b)
          Nothing -> benchGen' lk1Mb lk2Mb parLimit n

\end{code}
