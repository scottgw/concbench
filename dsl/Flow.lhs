%include polycode.fmt
%format :+: = "\mathbf{\plus}"
%format :*: = "\times"
%format >>> = "\ggg"
%format *** = "\sss"
%format &&& = "\aaa"
%if False
\begin{code}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module Flow where

import           Control.Category
import Data.Maybe

import Prelude hiding ((.), id)

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck.Gen as Test
import qualified Test.QuickCheck.Arbitrary as Test
\end{code}
%endif
Since the domain specific language for benchmarks
must be abstract,
it builts on simple categorical structures.
For starters, every benchmark forms a category,
\begin{code}

\end{code}
The objects of this category are the types of data
that the benchmarks will consume or produce.
Here, |cat| refers to a morphism between objects.
The morphism operation |.| corresponds to  morphism composition  and 
|id| corresponds to the identity morphism.
These satisfy the normal categorical laws,
\[
\begin{array}{rcl}
\id \circ f & = & f \\
f \circ \id & = & f \\
f \circ (g \circ h) & = & (f \circ g) \circ h
\end{array}
\]
It's convenient to define a forward
composition operator which just reverses
of normal composition
\begin{code}
\end{code}

For a realization of this category for an imperative language
such as Java, it would be natural to define
|>>>| as function composition.
For example, |toString >>> length :: Object -> Integer| would become for example:
\begin{lstlisting}[language=java]
String s = obj.toString();
Integer n = s.length();
\end{lstlisting}
Assuming some input string. Of course in Java the data (obj, s, n)
could also be in argument position of a static call
rather than target position.
Similarly, for Java the |id| would become a no-op.

However, such a simple construction does not give
enough power to construct very interesting benchmarks.
For this we draw on the concept of the arrow from~\cite{hughes:2000:arrow},
which is a generalization of a monad.
This cannot be used directly though,
as the original formulation allows direct lifting of Haskell functions
via the |arr| method in the definition of the arrow.
One clearly cannot lift an arbitrary Haskell function into Java
without some serious work (compiling Haskell to Java),
so it is removed from the interface.

|arr| could be used to build more arrows,
consequently due to its removal
the |Arrow| must now explicitly define more operations
as they cannot be created via |arr| anymore.
Additionally, even operations that could be defined
via the remaining operations, such as |***|,
are still left in the interface
to allow implementations to override their behaviour directly.
This is in an effort to have some separation
between control flow operations like |>>>| and |***|
and type flow operations, such as |swap|, |first|, and |share|.
\begin{code}
class Category arrow => Arrow arrow where
  (***)  :: arrow a b -> arrow c d 
           -> arrow (a :*: c) (b :*: d)
  share  :: arrow a (a, a)
  first  :: arrow a b -> arrow (a :*: c) (b :*: c)
  swap   :: arrow (a :*: c) (c :*: a)
\end{code}
The relevant arrow laws have now changed to reflect
this modified interface,
and become:
\[
\begin{array}{rcl}
\first (f \ggg g) & = & \first f \ggg \first g \\
\swap \ggg \swap & = & \id \\
\share \ggg \swap & = & \share
\end{array}
\]

From here, 
a fair amount of other operations can be defined
by composition.
|second| is easy to define once 
the |swap| arrow and |first| function are available
\begin{code}
second :: Arrow arrow => 
          arrow a b -> arrow (c :*: a) (c :*: b)
second f = swap >>> first f >>> swap
\end{code}
and since it is common, we also add the |&&&| operator,
a share-parallel operator
\begin{code}
(&&&) :: Arrow arrow => arrow a b -> arrow a d
            -> arrow a (b :*: d)
f &&& g = share >>> (f *** g)
\end{code}

%if False
Since we are not especially concerned with the functional
correctness of the benchmarks
we also allow the following ``optimizations''
to reduce synchronization overheads, if necessary.
\[
\begin{array}{rcl}
(f \sss g) \ggg (h \sss i) & = & (f \ggg h) \sss (g \ggg i) \\
(f \aaa g) \ggg (h \sss i) & = & (f \ggg h) \aaa (g \ggg i) \\
\end{array}
\]

\begin{code}
infixr 3 ***
infixr 3 &&&
\end{code}
%endif
The previous definitions offer the base on which
the benchmarking arrows will be constructed.
Two operations are added to the base |Arrow|:
\begin{itemize}
 \item |sink| -- an arrow which just ``uses'' the incoming 
   data and produces a special type, |End|.
   The purpose is to denote the end of a computation.
 \item |forget1| -- allows data to be forgotten.
   This should be used carefully, one should ensure that this
   data was previously used in something with a side-effect 
   or it may be optimized away by the underlying
   compiler or runtime.
   |sink| would be the safer alternative,
   as it produces a side-effect for the given input data.
\end{itemize}

\begin{code}
class Arrow arrow => BenchArrow arrow where
  forgetEnd :: arrow (End :*: b) b
  sink  :: arrow a End
  source :: arrow Start Start
\end{code}
The purpose of this extension is to allow
benchmarks to express when they have completed.
It allows the arrows the note when some
data has been finished
(although there may be other references to the same data)
through |sink|,
and these finished pieces can be aggregated by using
|mergeEnd|.
These two operations produce the |End| type,
which we use to signal the end of a benchmark.
\begin{code}
type a :*: b = (a,b)
type a :+: b = Either a b
\end{code}
\begin{code}
data PreDef a = PreDef a
data Zero = Zero
data Succ a = Succ a
data End
data DoubleIdx
data Start
\end{code}

%if False
\begin{code}
data Eqq a b where
    Eqq :: Eqq a a

data TType a where
    TTEnd  :: TType End
    TTStart :: TType Start
    TTZero :: TType Zero
    TTDbl  :: TType DoubleIdx
    TTSucc :: TType a -> TType (Succ a)
    TTPair :: TType a -> TType b -> TType (a :*: b)

ttypeEq :: TType a -> TType b -> Bool
ttypeEq t1 t2 =
  case (t1, t2) of
    (TTEnd, TTEnd) -> True
    (TTZero, TTZero) -> True
    (TTDbl, TTDbl) -> True
    (TTPair a1 b1, TTPair a2 b2) -> ttypeEq a1 a2 && ttypeEq b1 b2
    (TTSucc a1, TTSucc a2) -> ttypeEq a1 a2
    _ -> False

toInt :: TType n -> Int
toInt i = 
  case i of
    TTSucc n -> 1 + toInt n
    _ -> 0

instance Show (TType a) where
  show t =
    case t of
      TTEnd -> "endT"
      TTStart -> "startT"
      TTZero -> "0T"
      TTDbl -> "dblT"
      TTSucc n -> show (toInt $ TTSucc n) ++ "T"
      TTPair t1 t2 -> concat ["(", show t1, ",", show t2, ")"]

data ArrowType a b where
    ArrCustom :: TType a -> TType b -> ArrowType a b
    ArrFirstType :: ArrowType a b -> TType c -> ArrowType (a :*: c) (b :*: c)
    ArrSwapType :: TType a -> TType b -> ArrowType (a :*: b) (b :*: a)
    ArrIdType :: TType a -> ArrowType a a
    ArrSinkType :: TType a -> ArrowType a End
    ArrAliasType :: TType a -> ArrowType (a :*: a) a
    ArrSplitType :: ArrowType a b -> ArrowType a d -> ArrowType a (b :*: d)
    ArrParType :: ArrowType a b -> ArrowType c d -> ArrowType (a :*: c) (b :*: d)
    ArrSeqType :: ArrowType a b -> ArrowType b c -> ArrowType a c

instance Show (ArrowType a b) where
  show arrType =
    concat [show (getInType arrType), " -> ", show (getOutType arrType)]

getOutType :: ArrowType a b -> TType b
getOutType (ArrCustom _a b) = b
getOutType (ArrFirstType a t) = TTPair (getOutType a) t
getOutType (ArrSwapType a b) = TTPair b a
getOutType (ArrIdType a  ) = a
getOutType (ArrSinkType _) = TTEnd
getOutType (ArrAliasType a) = a
getOutType (ArrSplitType a b) = TTPair (getOutType a) (getOutType b)
getOutType (ArrParType a b) = TTPair (getOutType a) (getOutType b)
getOutType (ArrSeqType _a b) = getOutType b

getInType :: ArrowType a b -> TType a
getInType (ArrCustom a _b) = a
getInType (ArrFirstType a t) = TTPair (getInType a) t
getInType (ArrSwapType a b) = TTPair a b
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

arrows1 :: BenchArrow arrow => [TType a -> TypedArrow arrow]
arrows1 = [ \ ty -> sink ::: ArrSinkType ty 
          , \ ty -> id ::: ArrIdType ty 
          ]


data ArrowSel =
  ArrowSelPar | ArrowSelSeq | ArrowSelSplit
  deriving (Bounded, Enum, Show)

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

class Arrow arrow => ArrowAlt arrow where
    (+++) :: arrow a b -> arrow c d -> arrow (a :+: c) (b :+: d)
    (|||) :: arrow a b -> arrow c b -> arrow (a :+: c) b

infixr 2 +++
infixr 2 |||

\end{code}
%endif
