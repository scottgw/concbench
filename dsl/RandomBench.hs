{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module RandomBench where

import Control.Category
import qualified Data.Map as Map
import Prelude hiding ((.), id)

import Flow
import FlowBench
import CustomBench
import FlowJava

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck.Gen as Test
import qualified Test.QuickCheck.Arbitrary as Test


type LockT = Succ Zero

ttLock :: TType LockT
ttLock = TTSucc TTZero

instance LockArrow TBench where
  type Lock TBench = LockT
  genLock = Extend "genLock" (ArrCustom TTStart ttLock) "Object"
  lock = Extend "lock" (ArrCustom ttLock ttLock) "Object"
  unlock = Extend "unlock" (ArrCustom ttLock ttLock) "Object"


protect :: (BenchArrow arrow, LockArrow arrow) =>
           arrow a b ->
           arrow (Start :*: a) b
protect f = 
  first (genLock >>> lock) >>>
  second f >>>
  first (unlock >>> sink) >>>
  forgetEnd


genLockArr :: TypedArrow TBench
genLockArr = genLock ::: ArrCustom TTStart ttLock


clqOfferE = Var "clqOffer" "boolean"

arrs :: [TypedArrow TBench]
arrs = [ genLockArr
       , lock ::: ArrCustom ttLock ttLock
       , unlock ::: ArrCustom ttLock ttLock
       , source ::: ArrCustom TTStart TTStart
       ]


arrows1 :: [TType a -> TypedArrow TBench]
arrows1 = [ \ ty -> sink ::: ArrSinkType ty 
          , \ ty -> id ::: ArrIdType ty           
          ]


runJava2 b = runJavaBench (Java b) (Map.singleton  "Object" (TTypeEx ttLock))

benches :: Int -> IO [TBench Start End]
benches n = Test.sample' (genBStartEnd arrs arrows1 TTStart TTEnd n)

javaBench n =
  do b:bs <- benches n
     print b
     runJavaBench (Java b) (Map.singleton  "Object" (TTypeEx ttLock))



arrowFilter1 :: -- forall arrow a b . 
                BenchArrow arrow
               => TType a
               -> TType b
               -> [TType a -> TypedArrow arrow]
               -> [Maybe (arrow a b)]
arrowFilter1 startTy endTy arrows1 = 
    map (\f -> pr (f startTy)) arrows1
    where
--        pr :: TypedArrow arrow -> Maybe (arrow a b)
        pr (f ::: ft) = 
            do Eqq <- testType startTy (getInType ft)
               Eqq <- testType endTy (getOutType ft)
               return f

ex1 :: Bool
ex1 = 
  case genLockArr of
    f ::: ft -> 
      case testType TTStart (getInType ft) of
        Just x -> True
        Nothing -> False
                  

