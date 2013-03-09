\documentclass{article}
%include polycode.fmt

\begin{document}
\begin{code}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad

import qualified Data.Traversable as Traverse
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UVM

import qualified Statistics.Sample as Stats

import           System.Environment()
import qualified System.Clock as Clock

import           Test.QuickCheck (Arbitrary, Property, Gen)
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Monadic as QuickCheck
\end{code}

\begin{code}
type Lock = MVar ()
type Memory = UVM.IOVector Int

timeAction :: IO a -> IO (Double, a)
timeAction act = do
  t1 <- Clock.getTime Clock.Monotonic -- getCurrentTime
  !r <- act
  t2 <- Clock.getTime Clock.Monotonic -- getCurrentTime
  let secDiff = Clock.sec t2 - Clock.sec t1
      nanoDiff = Clock.nsec t2 - Clock.nsec t1
      diff = fromIntegral secDiff + (fromIntegral nanoDiff / 10^(9::Int))
  return (diff, r)
\end{code}

Simple compositional benchmarks
\begin{code}

data Bench where
    BenFib   :: Bench
    BenLock1 :: Lock -> Bench -> Bench
    BenLock2 :: Lock -> Bench -> Bench
    BenSeq   :: !Bench -> !Bench -> Bench
    BenPar   :: !Bench -> !Bench -> Bench

instance Show Bench where
    show BenFib = "fib"
    show (BenLock1 _l b) = concat ["lock1(", show b, ")"]
    show (BenLock2 _l b) = concat ["lock2(", show b, ")"]
    show (BenSeq b1 b2) = concat ["(", show b1, ") ; (", show b2,")"]
    show (BenPar b1 b2) = concat ["(", show b1, ") ||| (", show b2,")"]

fib :: Int -> Int
fib n | n > 1     = fib (n-1) + fib (n-2)
      | otherwise = 1
                    
{-# NOINLINE fibM #-}
fibM :: Monad m => Int -> m ()
fibM n =
  let !_x = fib n
  in return ()

lock    :: Lock -> IO ()
lock    = void . takeMVar

unlock  :: Lock -> IO ()
unlock  = void . flip putMVar ()  

{-# NOINLINE compileBench #-}
compileBench :: Bench -> IO ()
compileBench BenFib  = fibM 32
compileBench (BenLock1 l b) = lock l >> compileBench b >> unlock l
compileBench (BenLock2 l b) = lock l >> compileBench b >> unlock l
compileBench (BenSeq b1 b2) = compileBench b1 >> compileBench b2
compileBench (BenPar b1 b2) = do
  barrier <- newEmptyMVar
  _ <- forkIO (compileBench b1 >> putMVar barrier ())
  _ <- forkIO (compileBench b2 >> putMVar barrier ())
  takeMVar barrier
  takeMVar barrier

{-# NOINLINE timeBench #-}
timeBench :: Bench -> IO Double
timeBench b = fst <$> (timeAction $ compileBench b)

instance Arbitrary Bench where
    arbitrary = QuickCheck.sized benchGen
    shrink = shrinkBench

shrinkBench :: Bench -> [Bench]
shrinkBench (BenPar a b) = [a, b] ++ do
  a' <- QuickCheck.shrink a
  b' <- QuickCheck.shrink b
  return (BenPar a' b')
shrinkBench (BenSeq a b) =  [a, b] ++ do
  a' <- QuickCheck.shrink a
  b' <- QuickCheck.shrink b
  return (BenSeq a' b')
shrinkBench (BenLock1 _ b) = [b]
shrinkBench (BenLock2 _ b) = [b]
shrinkBench _a           = []

data BenchSel = BenchSelPar | BenchSelSeq | BenchSelLock1| BenchSelLock2 
              deriving (Bounded, Enum)


benchGen :: Int -> Gen Bench
benchGen 0 = return BenFib
benchGen 1 = return BenFib
benchGen n = do
  switch :: Int <- QuickCheck.arbitrary
  let op = if switch `rem` 2 == 0 then BenSeq else BenPar 
      l = (n `div` 2) + n `rem` 2
      r = (n `div` 2)
  op <$> benchGen l <*> benchGen r

benchGenPar :: Maybe Lock -> Maybe Lock -> Int -> Int -> Gen Bench
benchGenPar lk1 lk2 parLimit n = snd <$> benchGenPar' lk1 lk2 parLimit n

benchGenPar' :: Maybe Lock -> Maybe Lock -> Int -> Int -> Gen (Int, Bench)
benchGenPar' _ _ _ 0 = return (0, BenFib)
benchGenPar' _ _ _ 1 = return (0, BenFib)
benchGenPar' lk1Mb lk2Mb parLimit n = do
  sel <- QuickCheck.arbitraryBoundedEnum
  let lSize = (n `div` 2) + n `rem` 2
      rSize = (n `div` 2)
  case sel of
    BenchSelPar -> 
        if parLimit == 0
        then do
          b <- BenSeq <$> (snd <$> benchGenPar' lk1Mb lk2Mb 0 lSize) 
                      <*> (snd <$> benchGenPar' lk1Mb lk2Mb 0 rSize)
          return (0,b)          
        else do
          let parLimit' = parLimit - 1
          (lNumPar, l) <- benchGenPar' lk1Mb lk2Mb parLimit' lSize
          let parLimit'' = parLimit' - lNumPar
          (rNumPar, r) <- benchGenPar' lk1Mb lk2Mb parLimit'' rSize
          return (lNumPar + rNumPar + 1,  BenPar l r)
    BenchSelSeq -> do
             (lPar, b1) <- benchGenPar' lk1Mb lk2Mb parLimit lSize
             (rPar, b2) <- benchGenPar' lk1Mb lk2Mb (parLimit - lPar) lSize
             return (lPar + rPar, BenSeq b1 b2)
    BenchSelLock1 -> 
        case lk1Mb of
          Just lk1 -> 
              do
                (p, b) <- benchGenPar' Nothing lk2Mb parLimit (n-1)
                return (p, BenLock1 lk1 b)
          Nothing -> benchGenPar' lk1Mb lk2Mb parLimit n
    BenchSelLock2 -> 
        case lk2Mb of
          Just lk2 -> 
              do
                (p, b) <- benchGenPar' lk1Mb Nothing parLimit (n-1)
                return (p, BenLock2 lk2 b)
          Nothing -> benchGenPar' lk1Mb lk2Mb parLimit n

{- # NOINLINE timeEstimation #-}
timeEstimation :: Bench -> IO Double
timeEstimation (BenPar a b)  = max <$> timeEstimation a <*> timeEstimation b
timeEstimation (BenSeq a b)  = (+) <$> timeEstimation a <*> timeEstimation b
timeEstimation (BenLock1 _lk1 b) = timeEstimation b
timeEstimation (BenLock2 _lk2 b) = timeEstimation b
timeEstimation b@BenFib = timeBench b
\end{code}

Random checking of tests
\begin{code}
estimate :: Bool -> Bench -> IO Bool
estimate verbose b = do
  estim <- runtimes (timeEstimation b)
  real  <- runtimes (timeBench b)
  let res = threshold estim real -- tTest 0.01 estim real
  when (res && verbose) $ do
    putStrLn (show (filterMean estim, filterMean real)) -- , estim, real))
  return res
          
    where
      numRuns :: Int
      numRuns = 20

      toUnboxed :: UV.Unbox a => V.Vector a -> UV.Vector a
      toUnboxed = UV.fromList . V.toList

      {--- # NOINLINE runtimes #-}
      runtimes :: IO Double -> IO (UV.Vector Double)
      runtimes act = toUnboxed <$> V.tail <$> 
                     Traverse.sequenceA (V.replicate (numRuns + 1) act)

filterMean :: UV.Vector Double -> Double
filterMean = Stats.mean . setOp (Set.deleteMin . Set.deleteMax)
  where
    setOp f = UV.fromList . Set.toList . f . Set.fromList . UV.toList

threshold :: UV.Vector Double -> UV.Vector Double -> Bool
threshold a b = 
    let
        x1 = filterMean a
        x2 = filterMean b
    in 
      abs (max x1 x2 / min x1 x2) > 1.10

numPar :: Bench -> Int
numPar (BenPar b1 b2) = 1 + numPar b1 + numPar b2
numPar (BenSeq b1 b2) = numPar b1 + numPar b2
numPar (BenLock1 _lk b) = numPar b
numPar (BenLock2 _lk b) = numPar b
numPar _ = 0

prop_withoutPar :: Maybe Lock -> Maybe Lock -> Int -> Property
prop_withoutPar lk1 lk2 n = 
  QuickCheck.forAllShrink 
     (QuickCheck.sized (benchGenPar lk1 lk2 n)) 
     shrinkBench 
     prop_estimation

prop_estimation :: Bench -> Property
prop_estimation = 
    \ b -> QuickCheck.monadicIO $ do
             r <- QuickCheck.run $ estimate False b
             QuickCheck.assert (not r)


-- runTests :: IO Bool
-- runTests = $quickCheckAll

main :: IO ()
main = do
  lk1 <- newMVar ()
  lk2 <- newMVar ()
  QuickCheck.quickCheck (prop_withoutPar (Just lk1) (Just lk2) 5)
  -- void runTests
  -- do
  --   -- fibM 37
  --   barrier <- newEmptyMVar
  --   forkIO (fibM 37 >> putMVar barrier ())
  --   forkIO (fibM 37 >> putMVar barrier ())
  --   takeMVar barrier
  --   takeMVar barrier
-- mutex2 :: B' (MVar ()) ()
-- mutex2 = MkB' (MkMem (newMVar ())) (MkExec 
\end{code}

\end{document}
