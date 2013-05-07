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
                 , TTypeEx(..)
                 , TBench(..)
                 , TType(..)
                 , TypedArrow(..)
                 , TypeMap
                 , ArrowMap
                 ) where

import           Control.Category

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
        Right e -> Right (e, typeMap, arrMap)
        Left str -> Left str

type TypeMap  = Map String TTypeEx
type ArrowMap = Map String (Maybe String, ArrowTypeEx, Maybe String)

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

typeCheck :: TypeMap -> ArrowMap -> UTBench -> Either String (TypedArrow TBench)
typeCheck typeMap arrMap expr =
  do TTypeEx t <- checkInType typeMap arrMap expr
     b ::^ t' <- typeCheck' typeMap arrMap t expr
     return (b ::: t')

defsToTypeMap :: [UTDef] -> TypeMap
defsToTypeMap ds = snd $ foldl go (TTypeEx TTZero, Map.empty) typeNames
  where
    typeName2 (UTDef _ t1 t2) = [t1, t2]
    
    typeNames :: [String] -- Either String String]
    typeNames = nub $ concatMap typeName2 ds
    
    go :: (TTypeEx, TypeMap) -> String -> (TTypeEx, TypeMap)
    go (TTypeEx typeIdx, typeMap) var = 
      let nextType = TTypeEx (TTSucc typeIdx)
      in (nextType, Map.insert var (TTypeEx typeIdx) typeMap)

defsToArrowMap :: [UTDef] -> TypeMap -> ArrowMap
defsToArrowMap defs typeMap = foldl go Map.empty defs
  where
    go arrMap (UTDef arrName inTypeStr outTypeStr) =
        case (t1ex, t2ex) of
          (TTypeEx t1, TTypeEx t2) ->
              Map.insert arrName ( justify inTypeStr
                                 , ArrowTypeEx (ArrCustom t1 t2)
                                 , justify outTypeStr) arrMap
      where
        justify "x" = Nothing
        justify str = Just str

        tmapLookup s =
            case Map.lookup s typeMap of
              Just t -> t
              _ -> error "defsToArrowMap: missing one of the types in the typeMap"
        t1ex = tmapLookup inTypeStr
        t2ex = tmapLookup outTypeStr
      -- case (Map.lookup inTypeStr typeMap, Map.lookup outTypeStr typeMap) of
      --   (Just (TTypeEx tIn), Just (TTypeEx tOut)) ->
      --     Map.insert arrName (inTypeStr, ArrowTypeEx (ArrCustom tIn tOut), outTypeStr) arrMap
      --   _ -> error "defsToArrowMap: missing one of the types in the typeMap"

testTypeEi t1 t2 =
    case testType t1 t2 of
      Just e -> Right e
      Nothing -> Left $ "types not equal: " ++ show t1 ++ "," ++ show t2

checkInType :: TypeMap -> ArrowMap -> UTBench -> Either String TTypeEx
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
         Eqq <- testTypeEi t1 t2
         return (TTypeEx t1)
    USource -> return (TTypeEx TTStart)
    UVar s ->
      do (_, ArrowTypeEx arrT, _) <- lookupEi s arrMap
         return (TTypeEx $ getInType arrT)
    _ -> Left $ "Cannot checkInType for " ++ show expr
  where
    go = checkInType typeMap arrMap

lookupEi s mp = 
    case Map.lookup s mp of
      Just i -> return i
      Nothing -> Left ("Not found in map: " ++ show s)

typeCheck' :: TypeMap -> ArrowMap -> TType n -> UTBench -> Either String (TypedBench n)
typeCheck' typeMap arrMap tIn expr =
  case expr of
    UNoop -> return (id ::^ ArrIdType tIn)
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
        _ -> Left "UPar didn't receive a pair"
    USplit b1 b2 ->
      do b1' ::^ b1Type <- go tIn b1
         b2' ::^ b2Type <- go tIn b2
         return ((b1' &&& b2') ::^ ArrSplitType b1Type b2Type)
    UFirst b ->
      case tIn of
        TTPair tIn1 tIn2 ->
          do b' ::^ bType <- go tIn1 b
             return ((first b') ::^ ArrFirstType bType tIn2)
        _ -> Left "UFirst didn't receive a pair"
    USwap ->
      case tIn of
        TTPair tIn1 tIn2 -> return (swap ::^ ArrSwapType tIn1 tIn2)
        _ -> Left "USwap didn't receive a pair"
    USink -> return (sink ::^ ArrSinkType tIn)
    UForgetEnd ->
      case tIn of
        TTPair TTEnd tIn2 -> return (forgetEnd ::^ ArrCustom tIn tIn2)
        _ -> Left "UForgetEnd didn't receive a pair of the form: (End, a)"
    UShare -> return (share ::^ ArrCustom tIn (TTPair tIn tIn))
    USource -> 
      case tIn of
        TTStart -> return (source ::^ ArrCustom TTStart TTStart)
        _ -> Left "source must have start input"
    UVar s ->
      do (inTypeStr, ArrowTypeEx arrT, Just outTypeStr) <- lookupEi s arrMap
         case inTypeStr of
           Nothing -> return (Var s outTypeStr ::^ ArrCustom tIn (getOutType arrT))
           Just _ ->
             do Eqq <- testTypeEi tIn (getInType arrT)
                let tOut    = getOutType arrT
                return (Var s outTypeStr ::^ ArrCustom tIn tOut)
    e -> error (show e)
  where
    go :: TType n -> UTBench -> Either String (TypedBench n)
    go = typeCheck' typeMap arrMap

data TBench a b where
  Var     :: String -> String -> TBench a b
  Noop    :: TBench a a
  Par     :: TBench a b -> TBench c d -> TBench (a :*: c) (b :*: d)
  Seq     :: TBench a b -> TBench b c -> TBench a c
  First   :: TBench a b -> TBench (a :*: c) (b :*: c)
  Swap    :: TBench (a :*: c) (c :*: a)
  Share   :: TBench a (a :*: a)

  Source  :: TBench Start Start
  Sink    :: TBench a End
  ForgetEnd :: TBench (End :*: c) c
  
  Extend  :: String -> ArrowType a b -> String -> TBench a b

instance Show (TBench n m) where
    show (Var s _) = s
    show Noop = "id"
    show (Par a b) = concat ["(",show a,") *** (", show b, ")"]
    show (Seq a b) = concat ["(",show a,") >>> (", show b, ")"]
    show (First a) = concat ["first (", show a, ")"]
    show Swap = "swap"
    show Share = "share"

    show Source = "source"
    show ForgetEnd = "forgetEnd"
    show Sink = "sink"

    show (Extend name _ _) = name
instance Show (TypedArrow TBench) where
    show (arr ::: ty) = concat [show arr, " :: ", show ty]

instance Show (TypedArrow1 TBench a) where
    show (arr ::! _ty) = show arr

instance Category TBench where
    (.) = flip Seq
    id  = Noop

instance Arrow TBench where
  (***) = Par
  share = Share
  first = First
  swap  = Swap

instance BenchArrow TBench where
  forgetEnd = ForgetEnd
  sink  = Sink
  source = Source
\end{code}
