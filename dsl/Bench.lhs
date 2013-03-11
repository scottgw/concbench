\begin{code}
module Bench where

import           Control.Concurrent

import           Criterion.Environment

import qualified Data.Vector.Unboxed.Mutable as UVM

import           Test.QuickCheck (Arbitrary)

import qualified Statistics.Resampling.Bootstrap as Stats


class Arbitrary a => Bench a where
    timeActual :: Environment -> a -> IO Stats.Estimate
    estimate :: BenchParams -> a -> Stats.Estimate
    lock1    :: Lock -> a -> a
    lock2    :: Lock -> a -> a
    (|>)     :: a -> a -> a
    (|||)    :: a -> a -> a


data BenchParams =
  BenchParams 
  { fibParam :: Stats.Estimate
  , lockParam :: Stats.Estimate
  , joinParam :: Stats.Estimate
  } deriving Show

type Lock = MVar ()
type Memory = UVM.IOVector Int


\end{code}
