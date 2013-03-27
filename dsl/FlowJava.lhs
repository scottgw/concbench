\begin{code}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module FlowJava where

import Data.Char

import           Control.Category
import           Control.Lens (makeLenses, view, over)

import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           Prelude hiding (id, (.))

import           System.Process

import           CustomBench
import           FlowBench
import           Flow

newtype Java a b = Java (TBench a b)

instance Category Java where
  Java f . Java g = Java $ g >>> f
  id              = Java id

instance Arrow Java where
  Java f *** Java g  = Java $ f *** g
  share = Java share
  swap  = Java swap
  first (Java f) = Java $ first f

instance BenchArrow Java where
  forgetEnd = Java forgetEnd
  sink = Java sink

instance LockArrow Java where
  type Lock Java   = PreDef Zero
  genLock          = Java $ Var "genLock" "Object"
  lock             = Java $ Var "lock" "Object"
  unlock           = Java $ Var "unlock" "Object"

instance MatrixArrow Java where
  type Matrix Java = PreDef (Succ Zero)
  genMatrix = Java $ Var "genMatrix" "array[][]"
  matMul    = Java $ Var "matMul" "array[][]"
  

data Args = Arg1 String | Arg2 Args Args deriving Show

varFromType :: Int -> String -> String
varFromType i t = map toLower t ++ show i

data Arg a where 
  ArgVar :: String -> String -> Arg a
  ArgTup :: String -> Arg a -> Arg b -> Arg (a :*: b)

toJavaType :: Arg a -> String
toJavaType (ArgVar _ typ) = typ
toJavaType (ArgTup _ a1 a2) = 
  unwords [ "Tuple", "<"
          , toJavaType a1
          , ","
          , toJavaType a2
          ]

instance Show (Arg a) where
  show arg =
    case arg of
      ArgVar s _type -> s
      ArgTup s _a1 _a2 -> s -- show a1 ++ ", " ++ show a2

data JavaDecl = 
  JavaDecl 
  { _declName :: String
  , _declType :: String
  } deriving (Eq, Ord)

data JavaState =
  JavaState 
  { _javaFresh :: Int
  , _javaDecls :: Set JavaDecl
  }

initState :: JavaState
initState = JavaState 0 Set.empty

data JavaResult =
  JavaResult
  { _resultState :: JavaState
  , _resultCode :: String
  }

makeLenses ''JavaDecl
makeLenses ''JavaState
makeLenses ''JavaResult

getFresh :: JavaState -> Int
getFresh = view javaFresh

incFresh :: JavaState -> JavaState
incFresh = over javaFresh (+1)

addDecl :: JavaDecl -> JavaState -> JavaState
addDecl d = over javaDecls (Set.insert d)

declString :: JavaDecl -> String
declString (JavaDecl name t) = unwords ["static", t, name,";"]

declsToJava :: Set JavaDecl -> String
declsToJava = unlines . map declString . Set.toList

parseAndCompileJava :: String -> Either String String
parseAndCompileJava str =
  case parseAndTypeCheck str of
    Left err -> Left err
    Right (arr, typeMap, _arrMap) -> Right $ compileJava arr typeMap

runTest :: String -> IO ()
runTest str = do
  case parseAndCompileJava str of
    Left err -> putStrLn err
    Right javaCode ->
      do generateByteCode "Test" javaCode
         times <- runByteCode "Test"
         print times

runJavaBench :: Java Start End -> TypeMap -> IO ()
runJavaBench (Java b) typeMap =
  do let str = compileJava (b ::: ArrCustom TTStart TTEnd) typeMap
     generateByteCode "Test" str
     times <- runByteCode "Test"
     print times

generateByteCode :: FilePath -> String -> IO ()
generateByteCode path str = do
  let javaPath = path ++ ".java"
      cp = "-cp commons-math-2.2.jar"
  writeFile javaPath str
  procHdl <- runCommand ("javac " ++ cp ++ " " ++ javaPath)
  _errCode <- waitForProcess procHdl
  return ()

runByteCode :: FilePath -> IO [Double]
runByteCode path = do
    output <- readProcess "java" ["-cp",cpOpt, "-server", path] ""
    case reads output of
      [] -> error output
      (x,_):_ -> return x
    where
      cpOpt = ".:commons-math-2.2.jar"

compileJava :: TypedArrow TBench -> TypeMap -> String
compileJava (a ::: _) _typeMap =
  let (state, code , _outArg) = 
        compileToJava (Java a)  (ArgVar "null" "Object") initState
      decls = view javaDecls state
  in wrapJavaBench decls code

compileToJava :: Java a b -> Arg a -> JavaState -> (JavaState, String, Arg b)
compileToJava (Java texpr) inArg state =
  case texpr of
    Var arrowName tOut -> lookupBinding state inArg arrowName tOut
    Extend arrowName t tOut -> lookupBinding state inArg arrowName tOut
    Seq f g -> (st2, unlines [code1, code2], outArg2)
      where          
        (st1, code1, outArg1) = go f inArg state
        (st2, code2, outArg2) = go g outArg1 (over javaFresh (+1) st1)
    Par f g ->
      case inArg of
        ArgTup _ a1 a2 ->
          let (st1, code1, outArg1) = go f a1 state
              (st2, code2, outArg2) = go g a2 st1
              tup = ArgTup ("tuple" ++ show (getFresh st2)) outArg1 outArg2
          in (st2, par code1 code2, tup)
    First f ->
      case inArg of 
        ArgTup name a1 a2 ->
          let (str, code, outArg) = go f a1 state
          in (str, code, ArgTup name outArg a2)
    ForgetEnd ->
      case inArg of
        ArgTup _ _a1 a2 -> (state, "", a2)
    Source ->
        (state, "", ArgVar "null" "Object")
    Sink ->
      (state, "", ArgVar "null" "Object")
    Share ->
      (state, "", ArgTup (error "reuse: argtup") inArg inArg)
    Swap ->
      case inArg of
        ArgTup name a1 a2 ->
          (state, "", ArgTup name a2 a1)
    Noop -> (state, "", inArg)
    e -> error (show e)
  where
    i = getFresh state

    go :: TBench a b -> Arg a -> JavaState -> (JavaState, String, Arg b)
    go e = compileToJava (Java e)

    par :: String -> String -> String
    par str1 str2 = 
      unlines [ "{"
              , thread 1 str1
              , thread 2 str2
              , "try {"
              , "  thread1.join();"
              , "  thread2.join();"
              , "} catch (Exception e) {"
              , "  e.printStackTrace();"
              , "}"
              , "}"
              ]

      where
            thread :: Int -> String -> String
            thread futNum str =
              unlines [unwords [ "Runnable", "runnable" ++ show futNum, "= new"
                               , "Runnable() {"
                               ]
                      , unlines [ "public void run(){"
                                , "  " ++ str
                                , "};"
                                ]
                      , "};"
                      , unwords ["Thread thread" ++ show futNum
                                , "= new Thread("
                                , "runnable" ++ show futNum
                                , ");"
                                ]
                      , "thread" ++ show futNum ++ ".start();"
                      ]

type VarLookup a b = JavaState -> Arg a -> String -> String -> (JavaState, String, Arg b)

lookupBinding :: VarLookup a b
lookupBinding state inArg arrowName outTypeName = 
    case arrowName of
      "lock" -> lockArrow state inArg arrowName outTypeName
      "unlock" -> unlockArrow state inArg arrowName outTypeName
      "clqOffer" -> dotVarObject "offer" state inArg arrowName outTypeName
      "clqPeek" -> dotVarObject "peek" state inArg arrowName outTypeName
      _      -> varArrow state inArg arrowName outTypeName

varArrow :: JavaState -> Arg a -> String -> String -> (JavaState, String, Arg b)
varArrow state inArg arrowName outTypeName =
    let varName = varFromType (view javaFresh state) outTypeName
        outArg = ArgVar varName outTypeName
        updState = addDecl (JavaDecl varName outTypeName) . incFresh
    in (updState state,
        unwords [varName, " = ", arrowName, "(", show inArg, ");"], 
        outArg)

dotVarObject :: String -> VarLookup a b
dotVarObject name state inArg arrowName outTypeName =
  let varName = varFromType (view javaFresh state) outTypeName
      outArg = ArgVar varName outTypeName
      updState = addDecl (JavaDecl varName outTypeName) . incFresh
      ivar = "i" ++ show (view javaFresh state)
  in (updState state,
      unlines [ "for (int " ++ ivar ++ "; " ++ ivar ++ " < 10000; " ++ ivar ++ "++){"
              , concat[varName, "=", show inArg, ".", name, "(new Object());"]
              , "}"
              ],
      outArg)
     
lockArrow :: VarLookup a b
lockArrow state inArg arrowName outTypeName =
    case inArg of
      ArgVar n t -> (state, unwords ["synchronized(", show inArg, "){"], ArgVar n t)
      _ -> error "lockArrow:"
unlockArrow :: VarLookup a b
unlockArrow state inArg arrowName outTypeName =
    case inArg of
      ArgVar n t -> (state, "}", ArgVar n t)
      _ -> error "unlockArrow:"

wrapJavaBench :: Set JavaDecl -> String -> String
wrapJavaBench decls block = 
    unlines (["import java.util.concurrent.*;"
             ,"import java.util.*;"
             ,"class Test {"
             ,"   static int fib (Object o) {"
             ,"     return fibR(35);"
             ,"   }"
             ,"   static int fibR (int i) {"
             ,"     return (i <= 1) ? 1 : fibR (i-1) + fibR(i-2);"
             ,"   }"
             ,"   static Object genLock (Object a) {"
             ,"     return new Object();"
             ,"   }"
             ] ++ [declsToJava decls] ++ -- Set.toList methods ++ 
             ["  public static void main (String[] args) {"
             ,"    Vector<Double> stats = new Vector<Double>();"
             ,"    final int innerSize = 512;"
             ,"    final int outerSize = 128*innerSize;"
--             ,unlines (Set.toList decls)
             ,"    final Object lk1 = new Object();"
             ,"    final Object lk2 = new Object();"
             ,"    final int[][] memArray = new int[outerSize][];"
             ,"    for (int i = 0; i < outerSize; i++) {"
             ,"      memArray[i] = new int[innerSize];"
             ,"    }"
             ,"    for (int i = 0; i < 10; i++) {"
             ,"       long startTime = System.nanoTime();"
--             ,unlines (Set.toList setup)
             ,"      " ++ block
             ,"    }"
             ,"    for (int i = 0; i < 50; i++) {"
             ,"      for (int j = 0; j < outerSize; j++) {"
             ,"        memArray[j] = new int[innerSize];"
             ,"      }"
--             ,unlines (Set.toList setup)
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
\end{code}
