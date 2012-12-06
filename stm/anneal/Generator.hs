module Main where

import System.Environment

import City

main = do
  nArg:distArg:_ <- getArgs
  let dist = read distArg
      n = read nArg
  genInput n dist