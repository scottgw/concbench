{-# LANGUAGE GADTs #-}
module UBench where

data UTProgram =
  UTProgram [UTDef] UTBench

data UTDef 
    = UTDef String 
            String -- (Either String String)
            String -- (Either String String)
            deriving Eq

data UTBench 
    = UNoop
    | UPar UTBench UTBench  
    | USeq UTBench UTBench
    | USplit UTBench UTBench
    | UFirst UTBench
    | USwap    

    | UShare   

    | UForgetEnd
    | USink
    | USource

    | UVar String
      deriving (Show)
