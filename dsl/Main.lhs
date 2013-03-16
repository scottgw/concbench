\documentclass{article}
%include polycode.fmt

\begin{document}
\begin{code}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Monad

import           Criterion.Config
import           Criterion.Environment
import           Criterion.Monad

import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Ord
import qualified Data.Set as Set

import qualified Statistics.Resampling.Bootstrap as Stats

import           System.Environment
import           System.Random

import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Gen as QuickCheck

import Bench
import Chiz
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
decideBenchMb :: (RunnableBench a, Bench a) => 
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

collectStats :: forall a . 
                (Show a, Ord a, RunnableBench a, Bench a) => 
                Environment
             -> BenchParams a
             -> [a]
             -> Int
             -> IO (Map a (Double, Double))
collectStats env param atoms maxSteps = do
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
                                             benchGenAnd atomsGen 5)
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

genStats :: Environment
              -> JavaVarRepl
              -> [BenchDsl]
              -> IO BenchMap
genStats env repl testCases  = 
  snd <$> foldM step (0, Map.empty) testCases
    where
      step :: (Int, BenchMap) -> BenchDsl -> IO (Int, BenchMap)
      step (i, results) testCase = 
        do putStrLn (concat [ show (i+1) , " ", pretty testCase])
           real <- timeActualJavaWith repl env (Java testCase)
           return (i+1, Map.insert testCase real results)

printData :: (Show a, Show b) => Map a b -> String
printData = 
    unlines . map (\(a,b) -> concat [show a, " => ", show b]) . Map.toList

runChizInput :: Environment -> ChizInput -> IO ChizInput
runChizInput env chizIn =
  do putStrLn $ "Running: " ++ view chizTestName chizIn
     chizIn' <- mapMOf chizTestElements (mapM runChizElem) chizIn
     return chizIn'
  where
    testCases = view chizTestCases chizIn

    replX elem op DslVar = Just part
      where
        part = JavaSrcPart Set.empty
                           (view elementDecls elem)
                           (Set.singleton $ view elementReset elem)
                           (view opCode op)
    replX _ _   _     = Nothing
    
    runChizElem :: Element -> IO Element
    runChizElem elmnt = mapMOf elementOperations (mapM (runChizOp elmnt)) elmnt

    runChizOp :: Element -> Operation -> IO Operation
    runChizOp elem op =
      do res <- genStats env (replX elem op) testCases
         let op' = set opResults (Just res) op
         return op'

main :: IO ()
main = do
  fileName:_ <- getArgs
  env <- withConfig defaultConfig measureEnvironment

  chizBStr <- BS.readFile fileName

  let
    chizInMb = decode chizBStr

    atomsGen = map return [cache, DslFib, DslVar]
    genX :: QuickCheck.Gen Java
    genX = Java <$> QuickCheck.sized 
           (\sz -> 
                let g = benchGenAnd atomsGen 5 sz
                in QuickCheck.suchThat g hasVar)

    x = DslVar

    genList 0 _  _    _   = []
    genList n sz rand gen =
      let (_val, rand') = next rand
          x = QuickCheck.unGen gen rand sz
      in x : genList (n-1) sz rand' gen

    -- standardTests = 
    --   Java <$> [ x
    --            , cache
    --            , x ||| x
    --            , x |> x
    --            , x |> cache
    --            , x ||| cache
    --            , x ||| x ||| cache
    --            , x ||| x ||| x
    --            , x ||| x ||| x ||| x
    --            ]

    -- concLinkQueueRemoveChiz =
    --   Chiz
    --   { charName = "concurrent-linked-queue-remove.chiz"
    --   , charRepl = h0Remove
    --   , charResults = Nothing 
    --   }

    -- arrayBlockQueueRemoveChiz =
    --   Chiz
    --   { charName = "arrayed-blocking-queue-remove.chiz"
    --   , charRepl = h1Remove
    --   , charResults = Nothing 
    --   }

  --   runChiz chiz = 
  --     do res <- genStats env (replX $ charRepl chiz) standardTests
  --        let chiz' = chiz {charResults = Just res}
  --        writeFile (charName chiz') (show chiz')

  --   chizes = [concLinkQueueRemoveChiz, arrayBlockQueueRemoveChiz]
  
  -- mapM_ runChiz chizes
  -- stats <- collectStats env param [cache mem] (Just lk1) (Just lk2) 40
  case chizInMb of
    Just chizIn -> 
      do chizIn' <- runChizInput env chizIn
         BS.writeFile (fileName ++ ".out") (encode chizIn')
    Nothing -> putStrLn "Failed to read Chiz input file"
  
h0Remove = qOp " q1.poll();"
h1Remove = qOp " q2.poll();"

h0Offer = qOp " q1.offer(dummy);"
h1Offer = qOp " q2.offer(dummy);"

qOp o = unlines ["for (int qi = 0; qi < qN; qi++){", o, "}"]
\end{code}

\end{document}
