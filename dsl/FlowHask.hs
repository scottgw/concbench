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
{-# LANGUAGE BangPatterns #-}
module FlowHask where

import Control.Parallel
import Control.Monad.Par

import Flow

instance IdxCategory (->) where
  idIdx = id
  compIdx f g = \ !x ->
    let !y = g x
        !z = f y
    in z

instance IdxArrow (->) where
  f &&& g = \a -> 
     let b = f a
         d = g a
     in b `par` (d `pseq` (b, d))

  f *** g = \ (a, c) ->
    let b = f a
        d = g c
    in b `par` d `pseq` (b, d)

  first f = \ (a, c) ->
    (f a, c)

  swap = \ (x, y) -> (y, x)

class IdxArrow arrow => FibBench arrow where
  type FibType arrow

  fib :: arrow (FibType arrow) (FibType arrow)

instance FibBench (->) where
  type FibType (->) = Int
  
  fib = go
    where go n
            | n <= 1 = 1
            | otherwise = fib (n-1) + fib (n-2)

main = do
  -- let x = fib 38
  --     y = fib 39
  -- print (x,y)
  -- print (x `par` y `pseq` (x,y))
  print ((first fib >>> second fib) (37, 38))
  -- print ((fib *** fib) (37,38))