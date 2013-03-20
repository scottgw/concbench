\documentclass{article}
%include polycode.fmt

\begin{document}
\begin{code}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Applicative
import           Control.Lens (view, set, mapMOf)
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

import           Bench
import           Chiz
import           Dsl
import           Java
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
              -> BenchParams
              -> a
              -> IO (Maybe (Double, Double))
decideBenchMb env param b = do
  let estim = estimate param b
  real <- timeActual env b

  let 
      average :: [Double] -> Double
      average xs = sum xs / fromIntegral (length xs)
      estimPt = Stats.estPoint estim
      realPt  = average real
      reject  = threshRatio estimPt realPt

  if reject
  then return $ Just (estimPt, realPt)
  else return $ Nothing

threshRatio :: Double -> Double -> Bool
threshRatio x1 x2 = abs (max x1 x2 / min x1 x2) > 1.10

collectStats :: forall a . 
                (Show a, Ord a, RunnableBench a, Bench a) => 
                Environment
             -> BenchParams
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


      updateWithResultMb results _testCase Nothing = 
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

    replX elm op DslVar = Just part
      where
        part = JavaSrcPart Set.empty
                           (view elementDecls elm)
                           (Set.singleton $ view elementReset elm)
                           (view opCode op)
    replX _ _   _     = Nothing
    
    runChizElem :: Element -> IO Element
    runChizElem elmnt = 
      do putStrLn (view elementType elmnt)
         mapMOf elementOperations (mapM (runChizOp elmnt)) elmnt

    runChizOp :: Element -> Operation -> IO Operation
    runChizOp elm op =
      do putStrLn (view opName op)
         res <- genStats env (replX elm op) testCases
         let op' = set opResults (Just res) op
         return op'

main :: IO ()
main = do
  fileName:rest <- getArgs

  chizBStr <- BS.readFile fileName

  print rest

  let chizInMb = eitherDecode chizBStr
  case chizInMb of
    Right chizIn ->
      case rest of
        "csv":_ -> do
          putStrLn "Converting to CSV"
          writeChizCSV (fileName ++ ".csv") chizIn
        _ ->
          do env <- withConfig defaultConfig measureEnvironment
             chizIn' <- runChizInput env chizIn
             BS.writeFile (fileName ++ ".out") (encode chizIn')
    Left err -> putStrLn $ "Failed to read Chiz input file: " ++ err
\end{code}

\end{document}
