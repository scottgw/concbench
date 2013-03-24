\begin{code}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module FlowBench ( parseAndTypeCheck
                 , lookupTypeName
--                 , TypeIdx (..)
                 , TBench(..)
                 , TType(..)
                 , TypedArrow(..)
                 , TypeMap
                 , ArrowMap
                 ) where

import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe

import           Prelude hiding ((.), id)
import qualified Prelude as Pre  ((.), id)

import           Flow
import           UBench
import           FlowParse

parseAndTypeCheck :: String -> Either String (TypedArrow TBench, TypeMap, ArrowMap)
parseAndTypeCheck str =
  case parseFlow str of
    Left err -> Left (show err)
    Right (UTProgram defs uexpr) ->
      let typeMap = defsToTypeMap defs
          arrMap = defsToArrowMap defs typeMap
      in case typeCheck typeMap arrMap uexpr of
        Just e -> Right (e, typeMap, arrMap)
        Nothing -> Left "failed to typecheck"


type TypeMap  = Map String TTypeEx
type ArrowMap = Map String (String, ArrowTypeEx, String)

data TypedBench n =
  forall m . TBench n m ::^ ArrowType n m
  
instance Show (TypedBench n) where
  show (b ::^ t) = concat [show b, " :: ", show t]

data TTypeEx = forall n . TTypeEx (TType n)

instance Eq TTypeEx where
  TTypeEx t1 == TTypeEx t2 = t1 `ttypeEq` t2

lookupTypeName :: TTypeEx -> TypeMap -> String
lookupTypeName var typeMap =
  case nameMb of
    Just name -> name
  where
    typeList :: [(String, TTypeEx)]
    typeList = Map.toList typeMap
    
    nameMb :: Maybe String
    nameMb = lookup var (map (\(x,y) -> (y,x)) typeList)

data ArrowTypeEx = forall n m . ArrowTypeEx (ArrowType n m)     

typeCheck :: TypeMap -> ArrowMap -> UTBench -> Maybe (TypedArrow TBench)
typeCheck typeMap arrMap expr =
  do TTypeEx t <- checkInType typeMap arrMap expr
     b ::^ t' <- typeCheck' typeMap arrMap t expr
     return (b ::: t')

defsToTypeMap :: [UTDef] -> TypeMap
defsToTypeMap ds = snd $ foldl go (TTypeEx TTZero, Map.empty) typeNames
  where
    typeName2 (UTDef _ t1 t2) = [t1, t2]
    
    typeNames :: [String]
    typeNames = nub $ concatMap typeName2 ds
    
    go :: (TTypeEx, TypeMap) -> String -> (TTypeEx, TypeMap)
    go (TTypeEx typeIdx, typeMap) var = 
      let nextType = TTypeEx (TTSucc typeIdx)
      in (nextType, Map.insert var (TTypeEx typeIdx) typeMap)

defsToArrowMap :: [UTDef] -> TypeMap -> ArrowMap
defsToArrowMap defs typeMap = foldl go Map.empty defs
  where
    go arrMap (UTDef arrName inTypeStr outTypeStr) =
      case (Map.lookup inTypeStr typeMap, Map.lookup outTypeStr typeMap) of
        (Just (TTypeEx tIn), Just (TTypeEx tOut)) ->
          Map.insert arrName (inTypeStr, ArrowTypeEx (ArrCustom tIn tOut), outTypeStr) arrMap
        _ -> error "defsToArrowMap: missing one of the types in the typeMap"

checkInType :: TypeMap -> ArrowMap -> UTBench -> Maybe TTypeEx
checkInType typeMap arrMap expr =
  case expr of
    USeq b1 _b2 -> go b1
    UPar b1 b2 -> 
      do TTypeEx t1 <- go b1
         TTypeEx t2 <- go b2
         return (TTypeEx $ TTPair t1 t2)
    USplit b1 b2 ->
      do TTypeEx t1 <- go b1
         TTypeEx t2 <- go b2
         Eqq <- testType t1 t2
         return (TTypeEx t1)
    UTimeIt b -> go b
    UVar s ->
      do (_, ArrowTypeEx arrT, _) <- Map.lookup s arrMap
         return (TTypeEx $ getInType arrT)
    _ -> Nothing
  where
    go = checkInType typeMap arrMap

typeCheck' :: TypeMap -> ArrowMap -> TType n -> UTBench -> Maybe (TypedBench n)
typeCheck' typeMap arrMap tIn expr =
  case expr of
    UNoop -> return (idIdx ::^ ArrIdType tIn)
    USeq b1 b2 -> 
      do b1' ::^ b1Type <- go tIn b1
         b2' ::^ b2Type <- go (getOutType b1Type) b2
         return ((b1' >>> b2') ::^ ArrSeqType b1Type b2Type)
    UPar b1 b2 ->
      case tIn of
        TTPair tIn1 tIn2 -> 
          do b1' ::^ b1Type <- go tIn1 b1
             b2' ::^ b2Type <- go tIn2 b2
             return ((b1' *** b2') ::^ ArrParType b1Type b2Type)
        _ -> Nothing
    USplit b1 b2 ->
      do b1' ::^ b1Type <- go tIn b1
         b2' ::^ b2Type <- go tIn b2
         return ((b1' &&& b2') ::^ ArrSplitType b1Type b2Type)
    UFirst b ->
      case tIn of
        TTPair tIn1 tIn2 ->
          do b' ::^ bType <- go tIn1 b
             return ((first b') ::^ ArrFirstType bType tIn2)
        _ -> Nothing
    USwap ->
      case tIn of
        TTPair tIn1 tIn2 -> return (swap ::^ ArrSwapType tIn1 tIn2)
        _ -> Nothing
    USink -> return (sink ::^ ArrSinkType tIn)
    UTimeIt b ->
      do b' ::^ bType <- go tIn b
         return (time b' ::^ ArrCustom tIn (TTPair (getOutType bType) TTDbl))
    UForget1 ->
      case tIn of
        TTPair _tIn1 tIn2 -> return (forget1 ::^ ArrCustom tIn tIn2)
        _ -> Nothing
    UReuse -> return (reuse ::^ ArrCustom tIn (TTPair tIn tIn))
    UVar s ->
      do (_inTypeStr, ArrowTypeEx arrT, outTypeStr) <- Map.lookup s arrMap
         Eqq <- testType tIn (getInType arrT)
         let tOut    = getOutType arrT
         return (Var s outTypeStr ::^ ArrCustom tIn tOut)
  where
    go :: TType n -> UTBench -> Maybe (TypedBench n)
    go = typeCheck' typeMap arrMap

data TBench a b where
  Var     :: String -> String -> TBench a b
  Noop    :: TBench a a
  Par     :: TBench a b -> TBench c d -> TBench (a :*: c) (b :*: d)
  Seq     :: TBench a b -> TBench b c -> TBench a c
  Split   :: TBench a b -> TBench a d -> TBench a (b :*: d)
  First   :: TBench a b -> TBench (a :*: c) (b :*: c)
  Swap    :: TBench (a :*: c) (c :*: a)
  Forget1 :: TBench (a :*: c) c
  Reuse   :: TBench a (a :*: a)

  Sink    :: TBench a End
  TimeIt  :: TBench a b -> TBench a (b :*: DoubleIdx)

instance Show (TBench n m) where
    show (Var s _) = s
    show Noop = "id"
    show (Par a b) = concat ["(",show a,") *** (", show b, ")"]
    show (Seq a b) = concat ["(",show a,") >>> (", show b, ")"]
    show (Split a b) = concat ["(",show a,") &&& (", show b, ")"]
    show (First a) = concat ["first (", show a, ")"]
    show Swap = "swap"
    show Reuse = "reuse"
    show Forget1 = "forget1"

    show Sink = "sink"
    show (TimeIt b) = "time (" ++ show b ++ ")"

instance Show (TypedArrow TBench) where
    show (arr ::: ty) = concat [show arr, " :: ", show ty]

instance Show (TypedArrow1 TBench a) where
    show (arr ::! _ty) = show arr

instance IdxCategory TBench where
  idIdx   = Noop
  compIdx = flip Seq

instance IdxArrow TBench where
  (***) = Par
  (&&&) = Split
  first = First
  swap  = Swap

instance BenchArrow TBench where
  forget1 = Forget1
  reuse = Reuse
  sink  = Sink
  time = TimeIt
\end{code}