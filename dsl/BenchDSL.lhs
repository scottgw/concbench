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

import           Criterion
import           Criterion.Analysis
import           Criterion.Config
import           Criterion.Environment
import           Criterion.Monad

import qualified Data.Traversable as Traverse
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UVM

import qualified Statistics.Sample as Stats
import qualified Statistics.Resampling.Bootstrap as Stats

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
compileBench BenFib  = fibM 31
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

-- {- # NOINLINE timeEstimation #-}
-- timeEstimation :: Bench -> IO Double
-- timeEstimation (BenPar a b)  = max <$> timeEstimation a <*> timeEstimation b
-- timeEstimation (BenSeq a b)  = (+) <$> timeEstimation a <*> timeEstimation b
-- timeEstimation (BenLock1 _lk1 b) = timeEstimation b
-- timeEstimation (BenLock2 _lk2 b) = timeEstimation b
-- timeEstimation b@BenFib = timeBench b


directEstimation :: BenchParams -> Bench -> Stats.Estimate
directEstimation param ben =
  case ben of
    BenFib -> fibParam param
    BenLock1 _l b -> estim b + lockParam param
    BenLock2 _l b -> estim b + lockParam param
    BenSeq b1 b2 -> estim b1 + estim b2
    BenPar b1 b2 -> (estim b1 `max` estim b2) + joinParam param
  where
    estim = directEstimation param

\end{code}

Random checking of tests
\begin{code}
estimate :: Environment -> BenchParams -> Bool -> Bench -> IO Bool
estimate env param verbose b = do
  let estim = directEstimation param b
  real <- measureBench env b

  let reject = threshRatio (Stats.estPoint estim) (Stats.estPoint real) -- rejectH0 estim real -- tTest 0.01 estim real
  when (reject && verbose) $ do
    putStrLn (show (estim, real)) -- , estim, real))
  return reject
          
    where
      numRuns :: Int
      numRuns = 10

      toUnboxed :: UV.Unbox a => V.Vector a -> UV.Vector a
      toUnboxed = UV.fromList . V.toList

      {--- # NOINLINE runtimes #-}
      runtimes :: IO Double -> IO (UV.Vector Double)
      runtimes act = toUnboxed <$> V.tail <$> 
                     Traverse.sequenceA (V.replicate (numRuns + 1) act)

rejectH0 :: Stats.Estimate -> Stats.Estimate -> Bool
rejectH0 e1 e2 = not (Stats.estPoint e1 `inEstim` e2 ||
                    Stats.estPoint e2 `inEstim` e1)
  where
    inEstim :: Double -> Stats.Estimate -> Bool
    inEstim val e = Stats.estLowerBound e <= val && val <= Stats.estUpperBound e

filterMean :: UV.Vector Double -> Double
filterMean v = Stats.mean $ dropOutliers v
  where
    dropOutliers = filterIdx maxIdx . filterIdx minIdx
    maxIdx = UV.maxIndex v
    minIdx = UV.minIndex v
    filterIdx i = UV.ifilter (\i' _v -> i /= i')

threshold :: UV.Vector Double -> UV.Vector Double -> Bool
threshold a b = 
    let
        x1 = filterMean a
        x2 = filterMean b
    in threshRatio x1 x2
      
threshRatio x1 x2 = abs (max x1 x2 / min x1 x2) > 1.10

-- numPar :: Bench -> Int
-- numPar (BenPar b1 b2) = 1 + numPar b1 + numPar b2
-- numPar (BenSeq b1 b2) = numPar b1 + numPar b2
-- numPar (BenLock1 _lk b) = numPar b
-- numPar (BenLock2 _lk b) = numPar b
-- numPar _ = 0

prop_withoutPar :: Environment -> BenchParams -> Maybe Lock -> Maybe Lock -> Int -> Property
prop_withoutPar env param lk1 lk2 n = 
  QuickCheck.forAllShrink 
     (QuickCheck.sized (benchGenPar lk1 lk2 n)) 
     shrinkBench 
     (prop_estimation env param)

prop_estimation :: Environment -> BenchParams -> Bench -> Property
prop_estimation env param = 
    \ b -> QuickCheck.monadicIO $ do
             r <- QuickCheck.run $ estimate env param False b
             QuickCheck.assert (not r)


-- runTests :: IO Bool
-- runTests = $quickCheckAll

measureBench :: Environment -> Bench -> IO Stats.Estimate
measureBench env b = do
  sample <- sampleM
  anMean <$> analyseSample 0.95 sample 20
 where 
   sampleM = withConfig defaultConfig $ 
             runBenchmark env (compileBench b)

instance Num Stats.Estimate where
  Stats.Estimate av1 lower1 upper1 confLevel1 - 
    Stats.Estimate av2 lower2 upper2 confLevel2 =
      Stats.Estimate (av1 - av2) (lower1 - lower2) (upper1 - upper2) confLevel2
  Stats.Estimate av1 lower1 upper1 confLevel1 +
    Stats.Estimate av2 lower2 upper2 confLevel2 =
      Stats.Estimate (av1 + av2) (lower1 + lower2) (upper1 + upper2) confLevel2


instance Ord Stats.Estimate where
  compare
    (Stats.Estimate av1 lower1 upper1 confLevel1)
    (Stats.Estimate av2 lower2 upper2 confLevel2) = compare av1 av2

data BenchParams =
  BenchParams 
  { fibParam :: Stats.Estimate
  , lockParam :: Stats.Estimate
  , joinParam :: Stats.Estimate
  } deriving Show

main :: IO ()
main = do
  lk1 <- newMVar ()
  lk2 <- newMVar ()

  env <- withConfig defaultConfig measureEnvironment

  fibEstim <- measureBench env BenFib
  lockEstim <- measureBench env (BenLock1 lk1 BenFib)
  parEstim <- measureBench env (BenPar BenFib BenFib)
    
  let param = BenchParams fibEstim (lockEstim - fibEstim) (parEstim - fibEstim)
  -- print param
  
  QuickCheck.quickCheck (prop_withoutPar env param (Just lk1) (Just lk2) 5)
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
