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

type a :*: b = (a,b)
type a :+: b = Either a b

data TypeIdx  = Zero
              | Succ TypeIdx
              -- | TypeIdx :*: TypeIdx
              -- | TypeIdx :+: TypeIdx
              | PreDef TypeIdx
        --      | End
--              | DoubleIdx

data End
data DoubleIdx

class IdxCategory cat
  where
    compIdx :: cat b c -> cat a b -> cat a c
    idIdx  :: cat a a

(>>>) :: IdxCategory cat => 
         cat a b -> cat b c -> cat a c
(>>>) = flip compIdx

class IdxCategory arrow => IdxArrow arrow where
  (***) :: arrow a b -> arrow c d 
           -> arrow (a :*: c) (b :*: d)
  (&&&) :: arrow a b -> arrow a d 
           -> arrow a         (b :*: d)
  first :: arrow a b -> arrow (a :*: c) (b :*: c)
  swap  :: arrow (a :*: c) (c :*: a)

second :: IdxArrow arrow => 
          arrow a b -> arrow (c :*: a) (c :*: b)
second f = swap >>> first f >>> swap

infixr 3 ***
infixr 3 &&&

class IdxArrow arrow => BenchArrow arrow where
  reuse :: arrow a (a :*: a)
  forget1 :: arrow (a :*: b) b
  sink  :: arrow a End
  time  :: arrow a b -> arrow a (b :*: DoubleIdx)