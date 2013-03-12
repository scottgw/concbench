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
import Java
\end{code}

\begin{code}

\end{code}

Simple compositional benchmarks
\begin{code}

\end{code}

Random checking of tests
\begin{code}
decideBench :: Bench a => Environment -> BenchParams a -> Bool -> a-> IO Bool
decideBench env param verbose b = do
  let estim = estimate param b
  real <- timeActual env b

  let reject = threshRatio (Stats.estPoint estim) (Stats.estPoint real)
  when (reject && verbose) $ do
    putStrLn (show (estim, real)) -- , estim, real))
  return reject
      
threshRatio x1 x2 = abs (max x1 x2 / min x1 x2) > 1.10

prop_withoutPar :: Bench a =>
                   Environment
                -> BenchParams a
                -> Maybe Lock
                -> Maybe Lock
                -> Int
                -> Property
prop_withoutPar env param lk1 lk2 n = 
  QuickCheck.forAllShrink 
     (QuickCheck.sized (benchGen lk1 lk2 n)) 
     QuickCheck.shrink
     (prop_estimation env param)

prop_estimation :: Bench a => 
                   Environment
                -> BenchParams a
                -> (a -> Property)
prop_estimation env param = 
    \ b -> QuickCheck.monadicIO $ do
             r <- QuickCheck.run $ decideBench env param True b
             QuickCheck.assert (not r)

main :: IO ()
main = do
  lk1 <- newMVar ()
  lk2 <- newMVar ()

  env <- withConfig defaultConfig measureEnvironment

  fibEstim <- timeActual env (Java $ DslFib)
  lockEstim <- timeActual env (Java $ DslLock1 lk1 DslFib)
  parEstim <- timeActual env (Java $ DslPar DslFib DslFib)
    
  let param :: BenchParams Java = 
               BenchParams fibEstim (lockEstim - fibEstim) (parEstim - fibEstim)
  print param
  
  QuickCheck.quickCheck (prop_withoutPar env param (Just lk1) (Just lk2) 5)
\end{code}

\end{document}
