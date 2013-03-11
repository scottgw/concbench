\documentclass{article}
%include polycode.fmt

\begin{document}
\begin{code}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Concurrent
import           Control.Monad

import           Criterion.Config
import           Criterion.Environment
import           Criterion.Monad

import qualified Statistics.Resampling.Bootstrap as Stats

import           System.Environment()

import           Test.QuickCheck (Property)
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Monadic as QuickCheck

import Bench
import Dsl

\end{code}

\begin{code}

\end{code}

Simple compositional benchmarks
\begin{code}

\end{code}

Random checking of tests
\begin{code}
decideBench :: Bench a => Environment -> BenchParams -> Bool -> a-> IO Bool
decideBench env param verbose b = do
  let estim = estimate param b
  real <- timeActual env b

  let reject = threshRatio (Stats.estPoint estim) (Stats.estPoint real)
  when (reject && verbose) $ do
    putStrLn (show (estim, real)) -- , estim, real))
  return reject
      
threshRatio x1 x2 = abs (max x1 x2 / min x1 x2) > 1.10

prop_withoutPar :: Environment -> BenchParams -> Maybe Lock -> Maybe Lock -> Int -> Property
prop_withoutPar env param lk1 lk2 n = 
  QuickCheck.forAllShrink 
     (QuickCheck.sized (benchGenPar lk1 lk2 n)) 
     shrinkDsl
     (prop_estimation env param)

prop_estimation :: Bench a => Environment -> BenchParams -> a -> Property
prop_estimation env param = 
    \ b -> QuickCheck.monadicIO $ do
             r <- QuickCheck.run $ decideBench env param False b
             QuickCheck.assert (not r)

main :: IO ()
main = do
  lk1 <- newMVar ()
  lk2 <- newMVar ()

  env <- withConfig defaultConfig measureEnvironment

  fibEstim <- measureDsl env DslFib
  lockEstim <- measureDsl env (DslLock1 lk1 DslFib)
  parEstim <- measureDsl env (DslPar DslFib DslFib)
    
  let param = BenchParams fibEstim (lockEstim - fibEstim) (parEstim - fibEstim)
  -- print param
  
  QuickCheck.quickCheck (prop_withoutPar env param (Just lk1) (Just lk2) 5)
\end{code}

\end{document}
