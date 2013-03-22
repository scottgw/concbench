\newcommand{\sss}[0]{\star\hspace{-0.4em}\star\hspace{-0.4em}\star}
\newcommand{\aaa}[0]{\hspace{0.1em}\&\hspace{-0.4em}\&\hspace{-0.4em}\&\hspace{0.1em}}

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

{-# LANGUAGE ScopedTypeVariables #-}
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
\end{code}
%endif
Since the embedding is well-typed, we need a way
to transmit arbitrary types from the target language
to our embedding.
This is done by enumerating the types with
natural numbers, as below.
There are also constructors for 
product and sum types,
as well as ``special'' types that signify the end
of a benchmark and the time reporting as a
double precision floating point number.
\begin{code}
data TypeIdx  = Zero | Succ TypeIdx
          | TypeIdx :*: TypeIdx | TypeIdx :+: TypeIdx
          | End | DoubleIdx 
\end{code}
The definition of the standard |Category| is restricted to
operate only on the |TypeIdx| \emph{kind} (the type of types).
\begin{code}
class IdxCategory (cat :: TypeIdx -> TypeIdx -> *) 
  where
    (.) :: cat b c -> cat a b -> cat a c
    id  :: cat a a
\end{code}
It's convenient to define a forward
composition operator
\begin{code}
(>>>) :: IdxCategory cat => 
         cat a b -> cat b c -> cat a c
(>>>) = flip (.)
\end{code}
Similarly, we redefine the notion of |Arrow| to inherit
from the indexed category type-class.
The indexed arrows also cannot lift arbitrary Haskell functions;
as such the |arr :: Arrow arrow => (a -> b) -> arrow a b|
function was removed.
\begin{code}
class IdxCategory arrow => IdxArrow arrow where
  (***) :: arrow a b -> arrow c d 
           -> arrow (a :*: c) (b :*: d)
  (&&&) :: arrow a b -> arrow a d 
           -> arrow a         (b :*: d)
  first :: arrow a b -> arrow (a :*: c) (b :*: c)
  swap  :: arrow (a :*: c) (c :*: a)
\end{code}
Of course any instance of these clases must  
preserve the existing categorical laws
\[
\begin{array}{rcl}
id \circ f & = & f \\
f \circ id & = & f \\
f \circ (g \circ h) & = & (f \circ g) \circ h
\end{array}
\]
and also a reduced set of arrow laws
(removing those that describe the lifting |arr| operation)
\[
\begin{array}{rcl}
first (f \ggg g) & = & first f \ggg first g \\
\end{array}
\]
Since we are not especially concerned with the functional
correctness of the benchmarks
we also allow the following ``optimization''
to reduce synchronization overheads, if necessary.
\[
\begin{array}{rcl}
(f \sss g) \ggg (h \sss i) & = & (f \ggg h) \sss (g \ggg i) \\
(f \aaa g) \ggg (h \sss i) & = & (f \ggg h) \aaa (g \ggg i) \\
\end{array}
\]

|second| is easy to define once 
the |swap| arrow and |first| function are available
\begin{code}
second :: IdxArrow arrow => 
          arrow a b -> arrow (c :*: a) (c :*: b)
second f = swap >>> first f >>> swap
\end{code}
%if False
\begin{code}
infixr 3 ***
infixr 3 &&&
\end{code}
%endif
The previous definitions offer the base on which
the benchmarking arrows will be constructed.
New operations are present:
\begin{itemize}
 \item |sink| -- an arrow which just ``uses'' the incoming 
   data and produces a special value |End|.
   The purpose is to denote the end of a computation.
 \item |alias| -- creates a binding of two types of data to the 
   same instance.
 \item |time| -- runs a computation and times the execution.
\end{itemize}

\begin{code}
class IdxArrow arrow => BenchArrow arrow where
  sink  :: arrow a End
  alias :: arrow (b :*: b) b
  time  :: arrow a b -> arrow a (b :*: DoubleIdx)
\end{code}
An example of how the typeclasses above can be 
used to create custom benchmarks can be seen in the
type-class definition below:
\begin{code}
class BenchArrow arrow => CustomArrow arrow 
 where
  type Matrix arrow :: TypeIdx
  genMatrix :: arrow a (Matrix arrow)
  matMul :: 
    arrow (Matrix arrow :*: Matrix arrow) (Matrix arrow)

  type Lock arrow :: TypeIdx
  genLock :: arrow a (Lock arrow)
  lock :: arrow a b -> arrow (Lock arrow :*: a) b
\end{code}
In this benchmark, there are two types of data
|Lock|s, and |Matrix|s, with associated operations.
A common pattern can be seen here,:
a benchmark is a combination of a data-generator
and some function which uses the generated data.
In the example above, 
there is no particular type that is required to 
create a |Matrix| or a |Lock|,
although they could take parameters.

The |lock| operation is unique as it is a function
which takes an arrow,
meaning that it transforms one arrow into another.
In particular, it transforms a regular arrow
into one that also requires a |Lock| to operate,
with the intension that the input argument
will now be protected by the |Lock|.
This can be seen below where two benchmarks
that use the same input data will now use
a lock to protect access to the structure.
\begin{code}
shareALock :: CustomArrow arrow =>
              arrow a b -> arrow a c -> arrow a (b :*: c)
shareALock f g = 
  (genLock &&& id) >>> (lock f &&& lock g)
\end{code}
Below we see an example of generating two matricies,
multiplying them both in parallel,
then multiplying those results again in parallel.
This example makes it clear that these
are not data-flow arrows, but type-flow arrows,
as there is no distinction between the same
data and the same \emph{type} of data,
although sometimes they can coincide.
\begin{code}
doubleMul :: CustomArrow arrow => 
             arrow a (Matrix arrow)
doubleMul =
  (genMatrix &&& genMatrix) >>> 
  (matMul &&& matMul) >>> 
  matMul
\end{code}

%if False
\begin{code}
data Eqq a b where
    Eqq :: Eqq a a

data TType :: TypeIdx -> * where
    TTEnd  :: TType End
    TTZero :: TType Zero
    TTDbl  :: TType DoubleIdx
    TTSucc :: TType a -> TType (Succ a)
    TTPair :: TType a -> TType b -> TType (a :*: b)

toInt :: TType n -> Int
toInt i = 
  case i of
    TTSucc n -> 1 + toInt n
    _ -> 0
  

instance Show (TType a) where
  show t =
    case t of
      TTEnd -> "endT"
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


class IdxArrow arrow => IdxArrowAlt arrow where
    (+++) :: arrow a b -> arrow c d -> arrow (a :+: c) (b :+: d)
    (|||) :: arrow a b -> arrow c b -> arrow (a :+: c) b

infixr 2 +++
infixr 2 |||

\end{code}
%endif