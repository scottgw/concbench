\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Java where

import           Control.Applicative

import           Criterion.Environment
import           Criterion.Analysis

import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as UV


import           Test.QuickCheck (Arbitrary)
import qualified Test.QuickCheck as QuickCheck

import qualified Statistics.Resampling.Bootstrap as Stats
import           System.Process

import           Bench
import           Dsl

newtype Java = Java { unJava :: BenchDsl } 
             deriving (Ord, Eq)

instance Show Java where
    show (Java dsl) = show dsl

instance Read Java where
  readsPrec p = map (\ (dsl, rest) -> (Java dsl, rest)) . readsPrec p

instance Arbitrary Java where
    arbitrary = Java <$> QuickCheck.arbitrary
    shrink (Java dsl) = Java <$> QuickCheck.shrink dsl

instance Bench Java where
    genAtom   = Java <$> genAtom
    cache     = Java cache
    sleep     = Java sleep
    lock1 b   = Java (lock1 (unJava b))
    lock2 b   = Java (lock2 (unJava b))
    b1 |> b2  = Java (unJava b1 |> unJava b2)
    b1 ||| b2 = Java (unJava b1 ||| unJava b2)
    estimate  = estimateJava
    benchSize = benchSize . unJava
    normalize = Java . normalize . unJava

instance RunnableBench Java where
    timeActual = timeActualJava

estimateJava :: BenchParams -> Java -> Stats.Estimate
estimateJava params java = 
  estimateDsl params (unJava java)

timeActualJava :: Environment -> Java -> IO [Double]
timeActualJava = timeActualJavaWith (const Nothing)

timeActualJavaWith :: JavaVarRepl -> Environment -> Java -> IO [Double]
timeActualJavaWith this _env java = timeToRunWith this java

data JavaSrcPart = 
  JavaSrcPart 
  { javaMethods :: Set String
  , javaDecls :: Set String
  , javaSetup :: Set String
  , javaStr   :: String
  }

type JavaVarRepl = BenchDsl -> Maybe JavaSrcPart

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

runByteCode :: FilePath -> IO [Double]
runByteCode path = do
    output <- readProcess "java" ["-cp",cpOpt, "-server", path] ""
    case reads output of
      [] -> error output
      (x,_):_ -> return x
    where
      cpOpt = ".:commons-math-2.2.jar"


timeToRunWith :: JavaVarRepl -> Java -> IO [Double]
timeToRunWith this java = do
  let path = "Bench"
  generateByteCodeWith this java path
  runByteCode path

timeToRun :: Java -> IO [Double]
timeToRun = timeToRunWith (const Nothing)

javaToASTWith :: JavaVarRepl -> Java -> String
javaToASTWith this = wrapJavaBench . dslToASTWith this . unJava

wrapJavaBench :: JavaSrcPart -> String
wrapJavaBench (JavaSrcPart methods decls setup block) = 
    unlines (["import java.util.concurrent.*;"
             ,"import java.util.*;"
             ,"class Bench {"
             ] ++ Set.toList methods ++ 
             ["  public static void main (String[] args) {"

             ,"    Vector<Double> stats = new Vector<Double>();"
             ,"    final int innerSize = 512;"
             ,"    final int outerSize = 128*innerSize;"
             ,unlines (Set.toList decls)
             ,"    final Object lk1 = new Object();"
             ,"    final Object lk2 = new Object();"
             ,"    final int[][] memArray = new int[outerSize][];"
             ,"    for (int i = 0; i < outerSize; i++) {"
             ,"      memArray[i] = new int[innerSize];"
             ,"    }"
             ,"    for (int i = 0; i < 10; i++) {"
             ,"       long startTime = System.nanoTime();"
             ,unlines (Set.toList setup)
             ,"      " ++ block
             ,"    }"
             ,"    for (int i = 0; i < 50; i++) {"
             ,"      for (int j = 0; j < outerSize; j++) {"
             ,"        memArray[j] = new int[innerSize];"
             ,"      }"
             ,unlines (Set.toList setup)
             ,"      System.gc();"
             ,"      long startTime = System.nanoTime();"
             ,"      " ++ block
             ,"      long finishTime = System.nanoTime();"
             ,"      double duration = ((double)finishTime - (double)startTime)/1000000000.0;"
             ,"      stats.add (duration);"
             ,"    }"
             ,"    System.out.print('[');"
             ,"    for(int i = 0; i < stats.size() - 1; i++){"
             ,"      System.out.print (stats.get(i).toString());"
             ,"      System.out.print (',');"
             ,"    }"
             ,"    System.out.print(stats.get(stats.size()-1));"
             ,"    System.out.print(']');"
             ,"  }"
             ,"}"
             ]
            )

emptyJavaPart :: String -> JavaSrcPart
emptyJavaPart = JavaSrcPart Set.empty Set.empty Set.empty

singleJavaPart :: String -> String -> JavaSrcPart
singleJavaPart d = JavaSrcPart (Set.singleton d) Set.empty Set.empty

wrapPart :: (String -> String) -> JavaSrcPart -> JavaSrcPart
wrapPart wrapper part = part {javaStr = wrapper (javaStr part) }

unionPartsWith :: (String -> String -> String) 
                  -> JavaSrcPart -> JavaSrcPart -> JavaSrcPart
unionPartsWith f (JavaSrcPart mthds1 decls1 setup1 block1)
                 (JavaSrcPart mthds2 decls2 setup2 block2) =
  JavaSrcPart (mthds1 `Set.union` mthds2)
              (decls1 `Set.union` decls2)
              (setup1 `Set.union` setup2)
              (f block1 block2)

dslToASTWith :: JavaVarRepl -> BenchDsl -> JavaSrcPart
dslToASTWith this dsl =
    case this dsl of
      Just part -> part
      Nothing -> normalAST
    where
      normalAST = case dsl of
        DslVar -> error "dslToASTWith: should not find DslVar"
        DslFib -> singleJavaPart fibDef "fib(37);"
        DslSleep -> emptyJavaPart $
                    unlines ["for (int sleepi = 0; sleepi < 25; sleepi++){"
                            ,"  try {"
                            ,"    Thread.sleep(0,10000);"
                            ,"  } catch (Exception e) {"
                            ,"    e.printStackTrace();"
                            ,"  }"
                            ,"}"
                            ]
        DslCache -> 
            let code = unlines [ "for (int cacheI = 0; cacheI < outerSize; cacheI++) {"
                               , "  for (int cacheJ = 0; cacheJ < innerSize; cacheJ++) {"
                               , "    memArray[cacheI][cacheJ] = memArray[cacheI][cacheJ] * 2 + 1;"
                               , "  }"
                               , "}"
                               ]
            in emptyJavaPart code
        DslLock1 b ->
            let part = dslToAST b
            in  wrapPart (\ast -> unlines
                         ["synchronized (lk1) {"
                         ,"  " ++ ast
                         ,"}"
                         ]) part
        DslLock2 b ->
            let part = dslToAST b
            in  wrapPart (\ast -> unlines
                         ["synchronized (lk2) {"
                         ,"  " ++ ast
                         ,"}"
                         ]) part
        DslSeq b1 b2 -> 
          let part1 = dslToAST b1
              part2 = dslToAST b2
          in unionPartsWith (\ str1 str2 -> unlines [str1,str2]) part1 part2
        DslPar b1 b2 ->
          let part1 = dslToAST b1
              part2 = dslToAST b2
          in unionPartsWith (\ str1 str2 ->
              unlines [ "{"
                      , "Thread parThread1 = new Thread (new Runnable() {"
                      , "  public void run() {"
                      , "    " ++ str1
                      , "  }"
                      , "});"
                      , "Thread parThread2 = new Thread (new Runnable() {"
                      , "  public void run() {"
                      , "    " ++ str2
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
             ) part1 part2

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
