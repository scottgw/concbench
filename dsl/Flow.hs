{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE TypeFamilies #-}
module Flow where

import Control.Category

import Prelude hiding ((.), id)

data End

data B a b where
  Noop :: B a a
  Gen :: B ((), a) (b, c) -> B a (b, c)
  Par :: B a b -> B c d -> B (a, c) (b, d)
  Seq :: B a b -> B b c -> B a c
  Sink :: B a b -> B a End
  Swap :: B (a, c) (b, d) -> B (c, a) (d, b)
  First :: B a b -> B (a, c) (b, c)
  TimeIt :: B a b -> B a (b, Double)
  Alias :: B (b, b) b

class Category arrow => EmbedArrow arrow where
  (***) :: arrow a b -> arrow c d -> arrow (a, c) (b, d)
  (&&&) :: arrow a b -> arrow a d -> arrow a      (b, d)
  first :: arrow a b -> arrow (a, c) (b, c)
  swap  :: arrow (a, c) (b, d) -> arrow (c, a) (d, b)

second :: EmbedArrow arrow => arrow a b -> arrow (c, a) (c, b)
second = first >>> swap

infixr 3 ***
infixr 3 &&&

instance Category B where
  id  = Noop
  (.) = flip Seq

instance EmbedArrow B where
  (***)   = Par
  f &&& g = (f *** g) . share
  first   = First
  swap    = Swap

share :: EmbedArrow arrow => arrow a (a,a)
share = id &&& id

class EmbedArrow arrow => BenchArrow arrow where
  sink  :: arrow a b -> arrow a End
  alias :: arrow (b, b) b
  timeIt :: arrow a b -> arrow a (b, Double)

instance BenchArrow B where
  sink  = Sink
  timeIt = TimeIt
  alias = Alias

class BenchArrow arrow => CustomArrow arrow where
  type Lock arrow :: *
  genLock :: arrow a (Lock arrow)
  lock :: arrow a b -> arrow (lock, a) b

  type Matrix arrow :: *
  genMatrix :: arrow a (Matrix arrow)
  matrixMultiply :: arrow (Matrix arrow) (Matrix arrow)


shareALock :: CustomArrow arrow =>
              arrow a b ->
              arrow a c ->
              arrow a (b, c)
shareALock f g =
  (genLock &&& id) >>> (lock f &&& lock g)

doubleMul :: CustomArrow arrow => arrow a (Matrix arrow)
doubleMul = genMatrix >>> (matrixMultiply &&& matrixMultiply) >>> alias

-- class EmbedArrow arrow => EmbedArrowAlt arrow where
--     (+++) :: arrow a b -> arrow c d -> arrow (Either a c) (Either b d)
--     (|||) :: arrow a b -> arrow c b -> arrow (Either a c) b

-- instance EmbedArrowAlt arrow where
--     (+++)   = 

-- infixr 2 +++
-- infixr 2 |||
