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

newtype Java = Java { unJava :: BenchDsl String String } deriving (Ord, Eq)

instance Show Java where
    show (Java dsl) = show dsl

instance Arbitrary Java where
    arbitrary = Java <$> QuickCheck.arbitrary
    shrink (Java dsl) = Java <$> QuickCheck.shrink dsl

instance Bench Java String String where
    genAtom = Java <$> genAtom
    cache mem  = Java (cache mem)
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
estimateJava (BenchParams f c l j) java = 
  estimateDsl (BenchParams f c l j) (unJava java)

timeActualJava :: Environment -> Java -> IO Stats.Estimate
timeActualJava _env java = do
  (time, stddev) <- timeToRun java
  return (Stats.Estimate time (time - stddev) (stddev + time) 0.0)


timeActualJavaWith :: JavaVarRepl -> Environment -> Java -> IO Stats.Estimate
timeActualJavaWith this _env java = do
  (time, stddev) <- timeToRunWith this java
  return (Stats.Estimate time (time - stddev) (stddev + time) 0.0)


type JavaSrcPart = (Set String, String)
type JavaVarRepl = BenchDsl String String -> Maybe JavaSrcPart

generateByteCodeWith :: JavaVarRepl -> Java -> FilePath -> IO ()
generateByteCodeWith this java path = do
  let str = javaToASTWith this java
      javaPath = path ++ ".java"
      cp = "-cp commons-math-2.2.jar"
  writeFile javaPath str
  procHdl <- runCommand ("javac " ++ cp ++ " " ++ javaPath)
  _errCode <- waitForProcess procHdl
  return ()

generateByteCode :: Java -> FilePath -> IO ()
generateByteCode  = generateByteCodeWith (const Nothing)

runByteCode :: FilePath -> IO (Double, Double)
runByteCode path = 
    read <$> readProcess "java" ["-cp",cpOpt, path] ""
    where
      cpOpt = ".:commons-math-2.2.jar"


timeToRunWith :: JavaVarRepl -> Java -> IO (Double, Double)
timeToRunWith this java = do
  let path = "Bench"
  generateByteCodeWith this java path
  runByteCode path

timeToRun :: Java -> IO (Double, Double)
timeToRun = timeToRunWith (const Nothing)

javaToASTWith :: JavaVarRepl -> Java -> String
javaToASTWith this = uncurry wrapJavaBench . dslToASTWith this . unJava

wrapJavaBench :: Set String -> String -> String
wrapJavaBench methods block = 
    unlines (["import java.util.concurrent.*;"
             ,"import org.apache.commons.math.stat.descriptive.*;"
             ,"class Bench {"
             ] ++ Set.toList methods ++ 
             ["  public static void main (String[] args) {"
             ,"    DescriptiveStatistics stats = new DescriptiveStatistics();"
             ,"    final int innerSize = 512;"
             ,"    final int outerSize = 128*innerSize;"
             ,"    final Object dummy = new Object();"
             ,"    final ConcurrentLinkedQueue<Object> q1 = new ConcurrentLinkedQueue<Object>();"
             ,"    final LinkedBlockingQueue<Object> q2 = new LinkedBlockingQueue<Object>();"
             ,"    final Object lk1 = new Object();"
             ,"    final Object lk2 = new Object();"
             ,"    final int[][] memArray = new int[outerSize][];"
             ,"    for (int i = 0; i < outerSize; i++) {"
             ,"      memArray[i] = new int[innerSize];"
             ,"    }"
             ,"    final int qN = 100000;"
             ,"    for (int i = 0; i < qN*10; i++) { q1.offer (dummy); }"
             ,"    for (int i = 0; i < qN*10; i++) { q2.offer (dummy); }"
             ,"    for (int i = 0; i < 20; i++) {"
             ,"       long startTime = System.nanoTime();"
             ,"      " ++ block
             ,"    }"
             ,"    for (int i = 0; i < 50; i++) {"
             ,"      q1.clear();"
             ,"      q2.clear();"
             ,"      for (int j = 0; j < outerSize; j++) {"
             ,"        memArray[j] = new int[innerSize];"
             ,"      }"
             ,"      for (int j = 0; j < qN*10; j++) { q1.offer (dummy); }"
             ,"      for (int j = 0; j < qN*10; j++) { q2.offer (dummy); }"

             ,"      long startTime = System.nanoTime();"
             ,"      " ++ block
             ,"      long finishTime = System.nanoTime();"
             ,"      double duration = ((double)finishTime - (double)startTime)/1000000000.0;"
--             ,"      System.out.println (\"\" + duration);"
             ,"      stats.addValue (duration);"
             ,"    }"
             ,"    System.out.println(\"(\" + stats.getMean() + \",\" + stats.getStandardDeviation() + \")\");"
             ,"  }"
             ,"}"
             ]
            )

dslToASTWith :: JavaVarRepl -> BenchDsl String String -> JavaSrcPart
dslToASTWith this dsl =
    case this dsl of
      Just part -> part
      Nothing -> normalAST
    where
      normalAST = case dsl of
        DslFib -> (Set.singleton fibDef, "fib(37);")
        DslCache _mem -> 
            let code = unlines [ "for (int cacheI = 0; cacheI < outerSize; cacheI++) {"
                               , "  for (int cacheJ = 0; cacheJ < innerSize; cacheJ++) {"
                               , "    memArray[cacheI][cacheJ] = memArray[cacheI][cacheJ] * 2 + 1;"
                               , "  }"
                               , "}"
                               ]
            in (Set.empty, code)
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

      dslToAST = dslToASTWith this

fibDef :: String
fibDef = unlines ["  static int fib (int i) {"
                 ,"    if (i < 2)"
                 ,"      return 1;"
                 ,"    else"
                 ,"      return fib (i-1) + fib (i-2);"
                 ,"  }"
                 ]
\end{code}
