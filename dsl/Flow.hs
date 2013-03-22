{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-- # LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module Flow where

import Data.Maybe

import Prelude hiding ((.), id)
import qualified Prelude as Pre  ((.), id)

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck.Gen as Test
import qualified Test.QuickCheck.Arbitrary as Test

data Nat = Zero | Succ Nat | End | DoubleIdx | Nat :*: Nat

class IdxCategory (arrow :: Nat -> Nat -> *) where
  (.) :: arrow b c -> arrow a b -> arrow a c
  id  :: arrow a a

(>>>) :: IdxCategory arrow  => arrow a b -> arrow b c -> arrow a c
(>>>) = flip (.)

class IdxCategory (arrow :: Nat -> Nat -> *) => EmbedArrow arrow where
  (***) :: arrow a b -> arrow c d -> arrow (a :*: c) (b :*: d)
  (&&&) :: arrow a b -> arrow a d -> arrow a         (b :*: d)
  first :: arrow a b -> arrow (a :*: c) (b :*: c)
  swap  :: arrow (a :*: c) (c :*: a)

second :: EmbedArrow arrow => arrow a b -> arrow (c :*: a) (c :*: b)
second f = swap >>> first f >>> swap

infixr 3 ***
infixr 3 &&&

share :: EmbedArrow arrow => arrow a (a :*: a)
share = id &&& id

class EmbedArrow arrow => BenchArrow arrow where
  sink  :: arrow a End
  alias :: arrow (b :*: b) b
  timeIt :: arrow a b -> arrow a (b :*: DoubleIdx)

class BenchArrow arrow => CustomArrow arrow where
  type Lock arrow :: Nat
  genLock :: arrow a (Lock arrow)
  lock :: arrow a b -> arrow (Lock arrow :*: a) b

  type Matrix arrow :: Nat
  genMatrix :: arrow a (Matrix arrow)
  matrixMultiply :: arrow (Matrix arrow) (Matrix arrow)

shareALock :: CustomArrow arrow =>
              arrow a b ->
              arrow a c ->
              arrow a (b :*: c)
shareALock f g = (genLock &&& id) >>> 
                 (lock f &&& lock g)

doubleMul :: CustomArrow arrow => arrow a (Matrix arrow)
doubleMul = genMatrix >>> (matrixMultiply &&& matrixMultiply) >>> alias



-- -- genTest :: BenchArrow arrow => Int -> Gen (arrow () End)
-- -- genTest size | size <= 0 = return sink
-- -- genTest size =
-- --     do 

-- -- gen :: CustomArrow arrow => Int -> [arrow a b] -> Gen (arrow a b)
-- -- gen size arrows | size <= 0 = Test.elements arrows
-- -- gen size arrows = 
-- --     do sel <- Test.arbitraryBoundedEnum
-- --        case sel of
-- --          ArrowSelPar -> 
-- --              do f <- gen (size `div` 2 - 1) arrows
-- --                 g <- gen (size `div` 2 - 1) arrows
-- --                 return (share >>> f *** g >>> alias)
-- --          ArrowSelSeq ->
-- --              do f <- gen (size `div` 2 - 1) arrows
-- --                 g <- gen (size `div` 2 - 1) arrows
-- --                 return (f >>> g)

data Eqq a b where
    Eqq :: Eqq a a

data TType :: Nat -> * where
    TTEnd  :: TType End
    TTZero :: TType Zero
    TTSucc :: TType a -> TType (Succ a)
    TTPair :: TType a -> TType b -> TType (a :*: b)

data ArrowType a b where
    ArrCustom :: TType a -> TType b -> ArrowType a b
    ArrIdType :: TType a -> ArrowType a a
    ArrSinkType :: TType a -> ArrowType a End
    ArrAliasType :: TType a -> ArrowType (a :*: a) a
    ArrSplitType :: ArrowType a b -> ArrowType a d -> ArrowType a (b :*: d)
    ArrParType :: ArrowType a b -> ArrowType c d -> ArrowType (a :*: c) (b :*: d)
    ArrSeqType :: ArrowType a b -> ArrowType b c -> ArrowType a c

getOutType :: ArrowType a b -> TType b
getOutType (ArrCustom _a b) = b
getOutType (ArrIdType a  ) = a
getOutType (ArrSinkType _) = TTEnd
getOutType (ArrAliasType a) = a
getOutType (ArrSplitType a b) = TTPair (getOutType a) (getOutType b)
getOutType (ArrParType a b) = TTPair (getOutType a) (getOutType b)
getOutType (ArrSeqType _a b) = getOutType b

getInType :: ArrowType a b -> TType a
getInType (ArrCustom a _b) = a
getInType (ArrIdType a  ) = a
getInType (ArrSinkType a) = a
getInType (ArrAliasType a) = TTPair a a
getInType (ArrSplitType a _b) = getInType a
getInType (ArrParType a b) = TTPair (getInType a) (getInType b)
getInType (ArrSeqType a _b) = getInType a

testType :: TType n -> TType m -> Maybe (Eqq n m)
testType TTEnd TTEnd           = Just Eqq
testType TTZero TTZero         = Just Eqq
testType (TTSucc t) (TTSucc s) = do
  Eqq <- testType t s
  return Eqq
testType (TTPair p1 q1) (TTPair p2 q2) = do
  Eqq <- testType p1 p2
  Eqq <- testType q1 q2
  return Eqq
testType _ _ = Nothing

testInner :: ArrowType a b -> ArrowType c d -> Maybe (Eqq b c)
testInner f g = testType (getOutType f) (getInType g)

data TypedArrow arrow = forall a b . arrow a b ::: ArrowType a b

-- constArrows :: [TypedArrow arrow]
-- constArrows = [ swap ::: 
--               , sink ::: 
--               , alias ::: 
--               ]

arrows1 :: BenchArrow arrow => [TType a -> TypedArrow arrow]
arrows1 = [ \ ty -> sink ::: ArrSinkType ty 
          , \ ty -> id ::: ArrIdType ty 
          ]


data ArrowSel =
  ArrowSelPar | ArrowSelSeq | ArrowSelSplit
  deriving (Bounded, Enum, Show)


-- genB :: forall arrow . (BenchArrow arrow)
--         => [TypedArrow arrow]
--         -> Int
--         -> Gen (TypedArrow arrow)
-- genB arrows size | size <= 1 = Test.elements arrows
-- genB arrows size  = 
--     do sel <- Test.arbitraryBoundedEnum
--        case sel of
--          ArrowSelPar ->
--              do i <- Test.choose (1, size - 1)
--                 let i' = size - i
--                 f ::: ft <- genB arrows i
--                 g ::: gt <- genB arrows i'
--                 return ((f *** g) ::: ArrParType ft gt)
--          ArrowSelSeq ->
--              do f ::: ft <- genB arrows (size - 1)
--                 case startArrows (getOutType ft) arrows of 
--                   [] -> genB arrows size 
--                   as -> 
--                     do g ::! fg  <- Test.elements as
--                        return ((f >>> g) ::: ArrSeqType ft fg)
--          -- ArrowSelSplit ->
--          --     do i <- Test.choose (1, size - 1)
--          --        let i' = size - i
--          --        f ::: ft <- genB arrows i
--          --        g ::: gt <- genB arrows i'
--          --        return ((f &&& g) ::: ArrSplitType ft gt)

genBStartEnd :: forall arrow a b . (BenchArrow arrow)
        => [TypedArrow arrow]
        -> TType a
        -> TType b
        -> Int
        -> Gen (arrow a b)
genBStartEnd arrows startTy endTy size | size <= 1 = Test.elements (arrowFilter startTy endTy arrows)
genBStartEnd arrows startTy endTy size  = 
    do sel <- Test.arbitraryBoundedEnum
       case sel of
         ArrowSelPar ->
             case (startTy, endTy) of
               (TTPair s1 s2, TTPair e1 e2) ->
                   do i <- Test.choose (1, size - 1)
                      let i' = size - i
                      f <- genBStartEnd arrows s1 e1 i
                      g <- genBStartEnd arrows s2 e2 i'
                      return (f *** g)
               _ -> genBStartEnd arrows startTy endTy size
         ArrowSelSeq ->
             do i <- Test.choose (1, size - 1)
                let i' = size - i
                f ::! ft <- genBStart    arrows startTy               i
                g        <- genBStartEnd arrows (getOutType ft) endTy i'
                return (f >>> g)
         ArrowSelSplit ->
             case endTy of
               TTPair e1 e2 ->
                   do i <- Test.choose (1, size - 1)
                      let i' = size - i
                      f <- genBStartEnd arrows startTy e1 i
                      g <- genBStartEnd arrows startTy e2 i'
                      return (f &&& g)
               _ -> genBStartEnd arrows startTy endTy size

genBStart :: forall arrow a . (BenchArrow arrow)
        => [TypedArrow arrow]
        -> TType a
        -> Int
        -> Gen (TypedArrow1 arrow a)
genBStart arrows ty size | size <= 1 = Test.elements (startArrows ty arrows)
genBStart arrows ty size  = 
    do sel <- Test.arbitraryBoundedEnum
       case sel of
         ArrowSelPar ->
             case ty of
               TTPair t1 t2 ->
                   do i <- Test.choose (1, size - 1)
                      let i' = size - i
                      f ::! ft <- genBStart arrows t1 i
                      g ::! gt <- genBStart arrows t2 i'
                      return ((f *** g) ::! ArrParType ft gt)
               _ -> genBStart arrows ty size
         ArrowSelSeq ->
             do i <- Test.choose (1, size - 1)
                let i' = size - i
                f ::! ft <- genBStart arrows ty              i
                g ::! gt <- genBStart arrows (getOutType ft) i'
                return ((f >>> g) ::! ArrSeqType ft gt)
         ArrowSelSplit ->
             do i <- Test.choose (1, size - 1)
                let i' = size - i
                f ::! ft <- genBStart arrows ty i
                g ::! gt <- genBStart arrows ty i'
                return ((f &&& g) ::! ArrSplitType ft gt)


data TypedArrow1 arrow a = forall b . arrow a b ::! ArrowType a b

startArrows :: forall arrow a . BenchArrow arrow
               => TType a
               -> [TypedArrow arrow]
               -> [TypedArrow1 arrow a]
startArrows ty arrs = catMaybes (map pr (arrs ++ map ($ ty) arrows1))
    where
        pr :: TypedArrow arrow -> Maybe (TypedArrow1 arrow a)
        pr (f ::: ft) = 
            do Eqq <- testType ty (getInType ft)
               return (f ::! ft)

arrowFilter :: forall arrow a b . BenchArrow arrow
               => TType a
               -> TType b
               -> [TypedArrow arrow]
               -> [arrow a b]
arrowFilter startTy endTy arrs = 
    catMaybes (map pr (arrs ++ map ($ startTy) arrows1))
    where
        pr :: TypedArrow arrow -> Maybe (arrow a b)
        pr (f ::: ft) = 
            do Eqq <- testType startTy (getInType ft)
               Eqq <- testType endTy (getOutType ft)
               return f


-- genB :: forall arrow . (EmbedArrow arrow)
--         => [TypedArrow arrow]
--         -> Int
--         -> Gen (TypedArrow arrow)
-- genB arrows size | size <= 0 = Test.elements arrows
-- genB arrows size  = 
--     do sel <- Test.arbitraryBoundedEnum
--        case sel of
--          ArrowSelPar ->
--              do f ::: ft <- genB arrows (size `div` 2 - 1)
--                 g ::: fg <- genB arrows (size `div` 2 - 1)
--                 return ((f *** g) ::: ArrParType ft fg)
--          ArrowSelSeq ->
--              do let gen' :: Int -> Gen (TypedArrow arrow)
--                     gen' = genB arrows
--                 f ::: ft <- gen' (size `div` 2 - 1)
--                 g ::: gt <- gen' (size `div` 2 - 1)
--                 case testInner ft gt of
--                   Just Eqq -> return ((f >>> g)  ::: ArrSeqType ft gt)
--                   Nothing -> gen' size


-- genWithSource :: forall arrow a . BenchArrow arrow
--                  => TType a
--                  -> Int
--                  -> Gen (TypedArrow1 arrow a)
-- genWithSource ty size | size <= 0 = Test.elements (startArrows ty)
-- genWithSource _ty _size =
--     do undefined

-- -- class EmbedArrow arrow => EmbedArrowAlt arrow where
-- --     (+++) :: arrow a b -> arrow c d -> arrow (Either a c) (Either b d)
-- --     (|||) :: arrow a b -> arrow c b -> arrow (Either a c) b

-- -- instance EmbedArrowAlt arrow where
-- --     (+++)   = 

-- -- infixr 2 +++
-- -- infixr 2 |||

data B :: Nat -> Nat -> * where
  Noop    :: B a a
  Par     :: B a b -> B c d -> B (a :*: c) (b :*: d)
  Seq     :: B a b -> B b c -> B a c
  Split   :: B a b -> B a d -> B a (b :*: d)
  First   :: B a b -> B (a :*: c) (b :*: c)
  Swap    :: B (a :*: c) (c :*: a)

  Sink    :: B a End
  TimeIt  :: B a b -> B a (b :*: DoubleIdx)
  Alias   :: B (b :*: b) b

  Custom1 :: B Zero (Succ Zero)
  Custom2 :: B (Succ Zero) (Succ (Succ Zero))

instance Show (B n m) where
    show Noop = "noop"
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

instance Show (TypedArrow B) where
    show (arr ::: _ty) = show arr

instance Show (TypedArrow1 B a) where
    show (arr ::! _ty) = show arr

genExample :: TType a -> TType b -> IO ()
genExample startTy endTy = Test.sample $ Test.sized (genBStartEnd bAtoms startTy endTy)

genExample' :: TType a -> TType b -> Int -> IO ()
genExample' startTy endTy n = Test.sample $ genBStartEnd bAtoms startTy endTy n


bAtoms :: [TypedArrow B]
bAtoms = [ Custom1 ::: ArrCustom TTZero (TTSucc TTZero) 
         , Custom2 ::: ArrCustom (TTSucc TTZero) (TTSucc (TTSucc TTZero))
         ]

instance IdxCategory B where
  id  = Noop
  (.) = flip Seq

instance EmbedArrow B where
  (***) = Par
  (&&&) = Split
  first = First
  swap  = Swap

instance BenchArrow B where
  sink  = Sink
  timeIt = TimeIt
  alias = Alias


-- instance Num Nat where
--   fromInteger i | i < 0 = error "Nat.fromIntegral: can't have natural less than zero"
--   fromInteger 0 = Zero
--   fromInteger i = Succ (fromInteger (i-1))
