{-# LANGUAGE GADTs #-}
module UBench where

data UTProgram =
  UTProgram [UTDef] UTBench

data UTDef = UTDef String String String deriving Eq

data UTBench where
  UNoop    :: UTBench
  UPar     :: UTBench -> UTBench -> UTBench
  USeq     :: UTBench -> UTBench -> UTBench
  USplit   :: UTBench -> UTBench -> UTBench
  UFirst   :: UTBench -> UTBench
  USwap    :: UTBench
  UForget1 :: UTBench
  UReuse   :: UTBench

  USink    :: UTBench
  UTimeIt  :: UTBench -> UTBench

  UVar     :: String -> UTBench