\documentclass{article}
%include polycode.fmt

\begin{document}
\begin{code}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Rank2Types #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

-- import           Control.Concurrent
import           Control.Applicative
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

import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Gen as QuickCheck

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
decideBenchMb :: (RunnableBench a, Bench a lock mem) => 
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

threshRatio :: Double -> Double -> Bool
threshRatio x1 x2 = abs (max x1 x2 / min x1 x2) > 1.10

collectStats :: forall a lock mem . 
                (Show a, Ord a, RunnableBench a, Bench a lock mem) => 
                Environment
             -> BenchParams a
             -> [a]
             -> Maybe lock
             -> Maybe lock
             -> Int
             -> IO (Map a (Double, Double))
collectStats env param atoms lk1Mb lk2Mb maxSteps = do
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
                     putStrLn (concat [ "shrunk "
                                      , show testCase
                                      , " to "
                                      , show testCase'
                                      ])
                     return $ Map.insert testCase' r' results
               _ -> 
                   do
                     putStrLn ("unshrinkable: " ++ show testCase)
                     return $ Map.insert testCase r results

      step i gen results
          | i >= maxSteps = return results
          | otherwise     = 
              do let (_val, gen') = next gen
                     atomsGen = map return atoms
                     testCase = QuickCheck.unGen 
                                  (QuickCheck.sized $ 
                                             benchGenAnd atomsGen lk1Mb lk2Mb 5)
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


type BenchMap a = Map a Stats.Estimate

genStats :: Environment
              -> JavaVarRepl
              -> [Java]
              -> IO (BenchMap Java)
genStats env repl testCases  = 
  snd <$> foldM step (0, Map.empty) testCases
    where
      step :: (Int, BenchMap Java) -> Java -> IO (Int, BenchMap Java)
      step (i, results) testCase = 
        do putStrLn (concat [ show (i+1) , " ", show testCase])
           real <- timeActualJavaWith repl env testCase
           return (i+1, Map.insert testCase real results)

printData :: (Show a, Show b) => Map a b -> String
printData = 
    unlines . map (\(a,b) -> concat [show a, " => ", show b]) . Map.toList

main :: IO ()
main = do
  lk1 <- return "Lock1" -- newMVar ()
  lk2 <- return "Lock2" -- newMVar ()
  mem <- return "MemArray"
  env <- withConfig defaultConfig measureEnvironment

  -- fibEstim <- timeActual env (Java $ DslFib)
  -- cacheEstim <- timeActual env (Java $ DslCache mem)
  -- lockEstim <- timeActual env (Java $ DslLock1 lk1 DslFib)
  -- parEstim <- timeActual env (Java $ DslPar DslFib DslFib)

  -- let param :: BenchParams Java = 
  --              BenchParams 
  --                 fibEstim 
  --                 cacheEstim 
  --                 (lockEstim - fibEstim) 
  --                 (parEstim - fibEstim)
  -- print ( Stats.estPoint (fibParam param)
  --       , Stats.estPoint (cacheParam param)
  --       , Stats.estPoint (lockParam param)
  --       , Stats.estPoint (joinParam param)
  --       )

  let
    atomsGen = map return [cache mem, DslFib, DslVar]
    genX :: QuickCheck.Gen Java
    genX = Java <$> QuickCheck.sized 
           (\sz -> 
                let g = benchGenAnd atomsGen (Just lk1) (Just lk2) 5 sz
                in QuickCheck.suchThat g hasVar)

--    x :: Gen (BenchDsl String String
    x = DslVar
    
    genList 0 _  _    _   = []
    genList n sz rand gen =
      let (_val, rand') = next rand
          x = QuickCheck.unGen gen rand sz
      in x : genList (n-1) sz rand' gen
    
    standardTests = 
      Java <$> [ x
               , cache mem
               , x ||| x
               , x |> x
               , x |> cache mem
               , x ||| cache mem
               , x ||| x ||| cache mem
               , x ||| x ||| x
               , x ||| x ||| x ||| x
               ]
    
  
    removeChiz0 = 
      Chiz
      { charName = "concurrent-linked-queue-remove.chiz"
      , charRepl = h0Remove
      , charResults = Nothing 
      }
      
    removeChiz1 = 
      Chiz
      { charName = "arrayed-blocking-queue-remove.chiz"
      , charRepl = h1Remove
      , charResults = Nothing 
      }
  
    replX str DslVar = Just (Set.empty, str)
    replX _   _      = Nothing

    runChiz chiz = 
      do res <- genStats env (replX $ charRepl chiz) standardTests
         let chiz' = chiz {charResults = Just res}
         writeFile (charName chiz') (show chiz')

  
  runChiz removeChiz0
  runChiz removeChiz1
  -- stats <- collectStats env param [cache mem] (Just lk1) (Just lk2) 40

deriving instance Read Stats.Estimate

-- | The characterization description and results.
data Chiz = 
  Chiz 
  { charName :: String
  , charRepl :: String
  , charResults :: Maybe (BenchMap Java)
  } deriving (Read, Show)

h0Remove = qOp " q1.poll();"
h1Remove = qOp " q2.poll();"

h0Offer = qOp " q1.offer(dummy);"
h1Offer = qOp " q2.offer(dummy);"

qOp o = unlines ["for (int qi = 0; qi < qN; qi++){", o, "}"]
\end{code}

\end{document}
