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

-- import           Control.Concurrent
import           Control.Monad

import           Criterion.Config
import           Criterion.Environment
import           Criterion.Monad

import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Ord
import qualified Data.Set as Set

import qualified Statistics.Resampling.Bootstrap as Stats

import           System.Environment()
import           System.Random

-- import           Test.QuickCheck (Property)
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Gen as QuickCheck
-- import qualified Test.QuickCheck.Monadic as QuickCheck

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
decideBenchMb :: (RunnableBench a, Bench a lock) => 
                 Environment
              -> BenchParams a
              -> a
              -> IO (Maybe (Double, Double))
decideBenchMb env param b = do
  let estim = estimate param b
  real <- timeActual env b

  let 
      estimPt = Stats.estPoint estim
      realPt  = Stats.estPoint real
      reject  = threshRatio estimPt realPt

  if reject
  then return $ Just (estimPt, realPt)
  else return $ Nothing

-- decideBench :: (RunnableBench a, Bench a lock) => 
--                Environment
--             -> BenchParams a
--             -> Bool
--             -> a
--             -> IO Bool
-- decideBench env param verbose b = do
--   resultMb <- decideBenchMb env param b
--   case resultMb of
--     Just (estim,real) -> print (b, estim, real) >> return True
--     Nothing           -> return False

threshRatio :: Double -> Double -> Bool
threshRatio x1 x2 = abs (max x1 x2 / min x1 x2) > 1.10

collectStats :: forall a lock . 
                (Show a, Ord a, RunnableBench a, Bench a lock) => 
                Environment
             -> BenchParams a
             -> Maybe lock
             -> Maybe lock
             -> Int
             -> IO (Map a (Double, Double))
collectStats env param lk1Mb lk2Mb maxSteps = do
  initGen <- newStdGen
  step 0 initGen Map.empty
    where
      shrink testCase =
          do let reduced = nub (QuickCheck.shrink testCase)
                 shrinks = sortBy (comparing benchSize) reduced
                 testShrink (Just res) _s = return (Just res)
                 testShrink Nothing s = 
                     do 
                        resultMb <- decideBenchMb env param s
                        putStrLn (show (s, resultMb))
                        case resultMb of
                          Just r -> return (Just (s, r))
                          Nothing -> return Nothing
             putStr (show (length shrinks) ++ " shrinks: ")
             res <- foldM testShrink Nothing shrinks
             putStrLn ""
             return res


      updateWithResultMb results testCase Nothing = 
          do 
             return results
      updateWithResultMb results testCase (Just r) =
          do !shrunkTestCase <- shrink testCase
             case shrunkTestCase of
               Just (testCase', r') -> 
                   do
                     putStrLn (concat ["shrunk ", show testCase, " to ", show testCase'])
                     return $ Map.insert testCase' r' results
               _ -> 
                   do
                     putStrLn ("unshrinkable: " ++ show testCase)
                     return $ Map.insert testCase r results

      step i gen results
          | i >= maxSteps = return results
          | otherwise     = 
              do let (_val, gen') = next gen
                     testCase = QuickCheck.unGen 
                                  (QuickCheck.sized $ 
                                             benchGen lk1Mb lk2Mb 5)
                                  gen
                                  10
                     shrinkSet = Set.fromList (QuickCheck.shrink testCase)
                     keySet = Set.fromList (Map.keys results)

                 if shrinkSet `Set.intersection` keySet /= Set.empty
                 then step i gen' results
                 else
                     do putStrLn (concat [ show (i+1) , "/", show maxSteps
                                         , " ", show testCase
                                         ])
                        resultMb <- decideBenchMb env param testCase
                        results' <- updateWithResultMb results 
                                      testCase resultMb
                        step (i+1) gen' results'
      

-- prop_estimation :: (RunnableBench a, Bench a lock) => 
--                    Environment
--                 -> BenchParams a
--                 -> (a -> Property)
-- prop_estimation env param = 
--     \ b -> QuickCheck.monadicIO $ do
--              r <- QuickCheck.run $ decideBench env param True b
--              QuickCheck.assert (not r)

-- prop_withoutPar :: (RunnableBench a, Bench a lock) =>
--                    Environment
--                 -> BenchParams a
--                 -> Maybe lock
--                 -> Maybe lock
--                 -> Int
--                 -> Property
-- prop_withoutPar env param lk1 lk2 n = 
--   QuickCheck.forAllShrink 
--      (QuickCheck.sized (benchGen lk1 lk2 n)) 
--      QuickCheck.shrink
--      (prop_estimation env param)


printData :: (Show a, Show b) => Map a b -> String
printData = 
    unlines . map (\(a,b) -> concat [show a, " => ", show b]) . Map.toList

main :: IO ()
main = do
  lk1 <- return "Lock1" -- newMVar ()
  lk2 <- return "Lock2" -- newMVar ()

  env <- withConfig defaultConfig measureEnvironment

  fibEstim <- timeActual env (Java $ DslFib)
  lockEstim <- timeActual env (Java $ DslLock1 lk1 DslFib)
  parEstim <- timeActual env (Java $ DslPar DslFib DslFib)
    
  let param :: BenchParams Java = 
               BenchParams fibEstim (lockEstim - fibEstim) (parEstim - fibEstim)
  print ( Stats.estPoint (fibParam param)
        , Stats.estPoint (lockParam param)
        , Stats.estPoint (joinParam param)
        )
  
  stats <- collectStats env param (Just lk1) (Just lk2) 20

  putStrLn (printData stats)

  --QuickCheck.quickCheck (prop_withoutPar env param (Just lk1) (Just lk2) 5)
\end{code}

\end{document}
