{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
module Flow where

import Control.Category

import Prelude hiding ((.))

data End

data B a b where
    Noop :: B a a
    Share :: B a (a,a)
    Par :: B a b -> B c d -> B (a, c) (b, d)
    Seq :: B a b -> B b c -> B a c
    Sink :: B a b -> B a End

class Category arrow => EmbedArrow arrow where
    share :: arrow a (a,a)
    (***) :: arrow a b -> arrow c d -> arrow (a, c) (b, d)
    (&&&) :: arrow a b -> arrow a d -> arrow a      (b, d)
    sink  :: arrow a b -> arrow a End

infixr 3 ***
infixr 3 &&&

instance Category B where
    id  = Noop
    (.) = flip Seq

instance EmbedArrow B where
    (***)   = Par
    f &&& g = (f *** g) . share
    share   = Share
    sink    = Sink

-- class EmbedArrow arrow => EmbedArrowAlt arrow where
--     (+++) :: arrow a b -> arrow c d -> arrow (Either a c) (Either b d)
--     (|||) :: arrow a b -> arrow c b -> arrow (Either a c) b

-- instance EmbedArrowAlt arrow where
--     (+++)   = 

-- infixr 2 +++
-- infixr 2 |||
