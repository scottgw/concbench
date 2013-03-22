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
module FlowBench where

import Prelude hiding ((.), id)
import qualified Prelude as Pre  ((.), id)

import qualified Test.QuickCheck.Gen as Test

import Flow

data UTBench where
  UNoop    :: UTBench
  UPar     :: UTBench -> UTBench -> UTBench
  USeq     :: UTBench -> UTBench -> UTBench
  USplit   :: UTBench -> UTBench -> UTBench
  UFirst   :: UTBench -> UTBench
  USwap    :: UTBench

  USink    :: UTBench
  UTimeIt  :: UTBench -> UTBench
  UAlias   :: UTBench

  UCustom1 :: UTBench
  UCustom2 :: UTBench

data TypedBench n =
  forall m . TBench n m ::^ ArrowType n m
  
instance Show (TypedBench n) where
  show (b ::^ t) = concat [show b, " :: ", show t]

data TTypeEx = forall n . TTypeEx (TType n)
     
typeCheck :: UTBench -> Maybe (TypedArrow TBench)
typeCheck expr =
  do TTypeEx t <- checkInType expr
     b ::^ t' <- typeCheck' t expr
     return (b ::: t')

checkInType :: UTBench -> Maybe TTypeEx
checkInType expr =
  case expr of
    USeq b1 _b2 -> checkInType b1
    UPar b1 b2 -> 
      do TTypeEx t1 <- checkInType b1
         TTypeEx t2 <- checkInType b2
         return (TTypeEx $ TTPair t1 t2)
    USplit b1 b2 ->
      do TTypeEx t1 <- checkInType b1
         TTypeEx t2 <- checkInType b2
         Eqq <- testType t1 t2
         return (TTypeEx t1)
    UTimeIt b -> checkInType b
    UCustom1 -> return (TTypeEx TTZero)
    UCustom2 -> return (TTypeEx $ TTSucc TTZero)
    _ -> Nothing

typeCheck' :: TType n -> UTBench -> Maybe (TypedBench n)
typeCheck' tIn expr =
  case expr of
    UNoop -> return (id ::^ ArrIdType tIn)
    USeq b1 b2 -> 
      do b1' ::^ b1Type <- typeCheck' tIn b1
         b2' ::^ b2Type <- typeCheck' (getOutType b1Type) b2
         return ((b1' >>> b2') ::^ ArrSeqType b1Type b2Type)
    UPar b1 b2 ->
      case tIn of
        TTPair tIn1 tIn2 -> 
          do b1' ::^ b1Type <- typeCheck' tIn1 b1
             b2' ::^ b2Type <- typeCheck' tIn2 b2
             return ((b1' *** b2') ::^ ArrParType b1Type b2Type)
        _ -> Nothing
    USplit b1 b2 ->
      do b1' ::^ b1Type <- typeCheck' tIn b1
         b2' ::^ b2Type <- typeCheck' tIn b2
         return ((b1' &&& b2') ::^ ArrSplitType b1Type b2Type)
    UFirst b ->
      case tIn of
        TTPair tIn1 tIn2 ->
          do b' ::^ bType <- typeCheck' tIn1 b
             return ((first b') ::^ ArrFirstType bType tIn2)
        _ -> Nothing
    USwap ->
      case tIn of
        TTPair tIn1 tIn2 -> return (swap ::^ ArrSwapType tIn1 tIn2)
        _ -> Nothing
    USink -> return (sink ::^ ArrSinkType tIn)
    UTimeIt b ->
      do b' ::^ bType <- typeCheck' tIn b
         return (time b' ::^ ArrCustom tIn (TTPair (getOutType bType) TTDbl))
    UAlias ->
      case tIn of
        TTPair tIn1 tIn2 -> 
          do Eqq <- testType tIn1 tIn2
             return (alias ::^ ArrCustom tIn tIn1)
        _ -> Nothing
    UCustom1 -> 
      case tIn of
        TTZero -> return (Custom1 ::^ ArrCustom tIn (TTSucc TTZero))
        _ -> Nothing
    UCustom2 -> 
      case tIn of
        TTSucc TTZero -> 
          return (Custom2 ::^ ArrCustom tIn (TTSucc (TTSucc TTZero)))
        _ -> Nothing

data TBench :: TypeIdx -> TypeIdx -> * where
  Noop    :: TBench a a
  Par     :: TBench a b -> TBench c d -> TBench (a :*: c) (b :*: d)
  Seq     :: TBench a b -> TBench b c -> TBench a c
  Split   :: TBench a b -> TBench a d -> TBench a (b :*: d)
  First   :: TBench a b -> TBench (a :*: c) (b :*: c)
  Swap    :: TBench (a :*: c) (c :*: a)

  Sink    :: TBench a End
  TimeIt  :: TBench a b -> TBench a (b :*: DoubleIdx)
  Alias   :: TBench (b :*: b) b

  Custom1 :: TBench Zero (Succ Zero)
  Custom2 :: TBench (Succ Zero) (Succ (Succ Zero))

instance Show (TBench n m) where
    show Noop = "id"
    show (Par a b) = concat ["(",show a,") *** (", show b, ")"]
    show (Seq a b) = concat ["(",show a,") >>> (", show b, ")"]
    show (Split a b) = concat ["(",show a,") &&& (", show b, ")"]
    show (First a) = concat ["first (", show a, ")"]
    show Swap = "swap"

    show Sink = "sink"
    show (TimeIt b) = "time (" ++ show b ++ ")"
    show Alias = "alias"

    show Custom1 = "custom1"
    show Custom2 = "custom2"

instance Show (TypedArrow TBench) where
    show (arr ::: ty) = concat [show arr, " :: ", show ty]

instance Show (TypedArrow1 TBench a) where
    show (arr ::! _ty) = show arr

genExample :: TType a -> TType b -> IO ()
genExample startTy endTy = Test.sample $ Test.sized (genBStartEnd bAtoms startTy endTy)

genExample' :: TType a -> TType b -> Int -> IO ()
genExample' startTy endTy n = Test.sample $ genBStartEnd bAtoms startTy endTy n


bAtoms :: [TypedArrow TBench]
bAtoms = [ Custom1 ::: ArrCustom TTZero (TTSucc TTZero) 
         , Custom2 ::: ArrCustom (TTSucc TTZero) (TTSucc (TTSucc TTZero))
         ]

instance IdxCategory TBench where
  id  = Noop
  (.) = flip Seq

instance IdxArrow TBench where
  (***) = Par
  (&&&) = Split
  first = First
  swap  = Swap

instance BenchArrow TBench where
  sink  = Sink
  time = TimeIt
  alias = Alias
\end{code}