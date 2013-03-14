\begin{code}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Bench where

import           Control.Applicative

import           Criterion.Environment

import           Test.QuickCheck (Arbitrary, Gen)
import qualified Test.QuickCheck as QuickCheck

import qualified Statistics.Resampling.Bootstrap as Stats


class RunnableBench a where
    timeActual :: Environment -> a -> IO Stats.Estimate

class (Show a, Arbitrary a) => Bench a lock mem | a -> lock, a -> mem where
    genAtom  :: Gen a
    estimate :: BenchParams a -> a -> Stats.Estimate
    cache    :: mem -> a
    lock1    :: lock -> a -> a
    lock2    :: lock -> a -> a
    (|>)     :: a -> a -> a
    (|||)    :: a -> a -> a
    benchSize :: a -> Int
    normalize :: a -> a

data BenchParams a =
  BenchParams 
  { fibParam :: Stats.Estimate
  , cacheParam :: Stats.Estimate
  , lockParam :: Stats.Estimate
  , joinParam :: Stats.Estimate
  }

instance Show (BenchParams a) where
    show params = concat ["fib: ", show $ fibParam params
                         ,"lock: ", show $ lockParam params
                         ,"join: ", show $ joinParam params
                         ]

data BenchSel = 
  BenchSelPar | BenchSelSeq | BenchSelLock1| BenchSelLock2 
  deriving (Bounded, Enum)

benchGen :: Bench a lock mem => mem -> Maybe lock -> Maybe lock -> Int -> Int -> Gen a
benchGen mem lk1 lk2 parLimit n = snd <$> benchGen' mem lk1 lk2 parLimit n

benchGen' :: Bench a lock mem 
             => mem -- ^ relevant memory instance for `cache`.
             -> Maybe lock -- ^ Lock #1, `Nothing` if it's already taken.
             -> Maybe lock -- ^ Lock #2, `Nothing` if it's already taken.
             -> Int 
             -> Int 
             -> Gen (Int, a)
benchGen' mem lk1Mb lk2Mb parLimit n = 
  benchGenAnd ([return $ cache mem]) lk1Mb lk2Mb parLimit n

benchGenAnd :: Bench a lock mem 
             => [Gen a]
             -> Maybe lock -- ^ Lock #1, `Nothing` if it's already taken.
             -> Maybe lock -- ^ Lock #2, `Nothing` if it's already taken.
             -> Int 
             -> Int 
             -> Gen (Int, a)
benchGenAnd others _ _ _ n
  | n <= 1 = (0,) <$> QuickCheck.oneof (genAtom:others)
benchGenAnd others lk1Mb lk2Mb parLimit n = do
  sel <- QuickCheck.arbitraryBoundedEnum
  let lSize = (n `div` 2) + n `rem` 2
      rSize = (n `div` 2)
  case sel of
    BenchSelPar -> 
        if parLimit == 0
        then benchGenAnd others lk1Mb lk2Mb parLimit n
        else do
          let parLimit' = parLimit - 1
          (lNumPar, l) <- benchGenAnd others lk1Mb lk2Mb parLimit' lSize
          let parLimit'' = parLimit' - lNumPar
          (rNumPar, r) <- benchGenAnd others lk1Mb lk2Mb parLimit'' rSize
          return (lNumPar + rNumPar + 1,  l ||| r)
    BenchSelSeq -> do
             (lPar, b1) <- benchGenAnd others lk1Mb lk2Mb parLimit lSize
             (rPar, b2) <- benchGenAnd others lk1Mb lk2Mb (parLimit - lPar) lSize
             return (lPar + rPar, b1 |> b2)
    BenchSelLock1 -> 
        case lk1Mb of
          Just lk1 -> 
              do
                (p, b) <- benchGenAnd others Nothing lk2Mb parLimit (n-1)
                return (p, lock1 lk1 b)
          Nothing -> benchGenAnd others lk1Mb lk2Mb parLimit n
    BenchSelLock2 -> 
        case lk2Mb of
          Just lk2 -> 
              do
                (p, b) <- benchGenAnd others lk1Mb Nothing parLimit (n-1)
                return (p, lock2 lk2 b)
          Nothing -> benchGenAnd others lk1Mb lk2Mb parLimit n

\end{code}
