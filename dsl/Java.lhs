\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Java where

import           Control.Applicative

import           Criterion.Environment

import           Data.Set (Set)
import qualified Data.Set as Set

import           Test.QuickCheck (Arbitrary)
import qualified Test.QuickCheck as QuickCheck

import qualified Statistics.Resampling.Bootstrap as Stats
import           System.Process

import           Bench
import           Dsl

newtype Java = Java { unJava :: BenchDsl String } deriving (Ord, Eq)

instance Show Java where
    show (Java dsl) = show dsl

instance Arbitrary Java where
    arbitrary = Java <$> QuickCheck.arbitrary
    shrink (Java dsl) = Java <$> QuickCheck.shrink dsl

instance Bench Java String where
    genAtom = Java <$> genAtom
    lock1 lk b = Java (lock1 lk (unJava b))
    lock2 lk b = Java (lock2 lk (unJava b))
    b1 |> b2   = Java (unJava b1 |> unJava b2)
    b1 ||| b2  = Java (unJava b1 ||| unJava b2)
    estimate   = estimateJava
    benchSize  = benchSize . unJava
    normalize  = Java . normalize . unJava

instance RunnableBench Java where
    timeActual = timeActualJava

estimateJava :: BenchParams Java -> Java -> Stats.Estimate
estimateJava (BenchParams f l j) java = 
  estimateDsl (BenchParams f l j) (unJava java)

timeActualJava :: Environment -> Java -> IO Stats.Estimate
timeActualJava _env java = do
  time <- timeToRun java
  return (Stats.Estimate time time time 0.0)

generateByteCode :: Java -> FilePath -> IO ()
generateByteCode java path = do
  let str = javaToAST java
      javaPath = path ++ ".java"
  writeFile javaPath str
  procHdl <- runCommand ("javac " ++ javaPath)
  _errCode <- waitForProcess procHdl
  return ()

runByteCode :: FilePath -> IO Double
runByteCode path = read <$> readProcess "java" [path] ""

timeToRun :: Java -> IO Double
timeToRun java = do
  let path = "Bench"
  generateByteCode java path
  runByteCode path
  

javaToAST :: Java -> String
javaToAST = uncurry wrapJavaBench . dslToAST . unJava

wrapJavaBench :: Set String -> String -> String
wrapJavaBench methods block = 
    unlines (["import java.util.concurrent.*;"
             ,"class Bench {"] ++ Set.toList methods ++ 
             ["  public static void main (String[] args) {"
             ,"    final Object lk1 = new Object();"
             ,"    final Object lk2 = new Object();"
             ,"    for (int i = 0; i < 10; i++) {"
             ,"      " ++ block
             ,"    }"
             ,"    long startTime = System.currentTimeMillis();"
             ,"    for (int i = 0; i < 20; i++) {"
             ,"      " ++ block
             ,"    }"
             ,"    long finishTime = System.currentTimeMillis();"
             ,"    System.out.println(\"\" + (((double)finishTime) - startTime)/1000.0/20);"
             ,"  }"
             ,"}"
             ]
            )
            

dslToAST :: BenchDsl String -> (Set String, String)
dslToAST dsl = 
    case dsl of
      DslFib -> (Set.singleton fibDef, "fib(37);")
      DslLock1 _lk b ->
          let (decls, bAst) = dslToAST b
          in (decls, unlines
                       ["synchronized (lk1) {"
                       ,"  " ++ bAst
                       ,"}"
                       ])
      DslLock2 _lk b ->
          let (decls, bAst) = dslToAST b
          in (decls, unlines
                       ["synchronized (lk2) {"
                       ,"  " ++ bAst
                       ,"}"
                       ])
      DslSeq b1 b2 -> 
          let (decls1, b1Ast) = dslToAST b1
              (decls2, b2Ast) = dslToAST b2
          in (decls1 `Set.union` decls2,
              unlines [b1Ast, b2Ast])
      DslPar b1 b2 ->
          let (decls1, b1Ast) = dslToAST b1
              (decls2, b2Ast) = dslToAST b2
          in (decls1 `Set.union` decls2,
              unlines [ "{"
                      , "Thread parThread1 = new Thread (new Runnable() {"
                      , "  public void run() {"
                      , "    " ++ b1Ast
                      , "  }"
                      , "});"
                      , "Thread parThread2 = new Thread (new Runnable() {"
                      , "  public void run() {"
                      , "    " ++ b2Ast
                      , "  }"
                      , "});"
                      , "parThread1.start();"
                      , "parThread2.start();"
                      , "try {"
                      , "  parThread1.join();"
                      , "  parThread2.join();"
                      , "} catch (Exception e) {"
                      , "  e.printStackTrace();"
                      , "}"
                      , "}"
                      ]
             )

fibDef :: String
fibDef = unlines ["  static int fib (int i) {"
                 ,"    if (i < 2)"
                 ,"      return 1;"
                 ,"    else"
                 ,"      return fib (i-1) + fib (i-2);"
                 ,"  }"
                 ]
\end{code}
