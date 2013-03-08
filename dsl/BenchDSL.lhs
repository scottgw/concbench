\documentclass{article}
%include polycode.fmt
%format >>> = "\rhd"
%format ==~ = "\approx"
\begin{document}
\begin{code}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad

import           Data.Time.Clock
import qualified Data.Traversable as Traverse
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UVM

import qualified Statistics.Sample as Stats

import           System.Environment

import           Test.QuickCheck
import           Test.QuickCheck.All (quickCheckAll)
import           Test.QuickCheck.Monadic
\end{code}

A benchmark is a task that produces quantifable results.
\begin{code}
newtype Benchmark a = Benchmark (IO a) 
                    deriving (Monad, Functor, Applicative, MonadIO)
\end{code}
%
Benchmark tasks also have a defined beginning and end.
For us, a metric is a function of a benchmark
%
\begin{code}
type Metric a = forall b . Benchmark b -> IO a
\end{code}
One common result is the time that the benchmark took to run.
\begin{code}
newtype Time = Time Double deriving (Eq, Ord, Num, Show)
\end{code}
This metric maybe described as `runtime`, rather
\begin{code}
runtime  ::  Metric Time
runtime  =  \ (Benchmark io) -> Time <$> fst <$> timeAction io
\end{code}
Benchmarks can be composed of different operations.
These are benchmark \emph{transformers}, or |BenchmarkT|
\begin{code}
type BenchmarkT a = Benchmark a -> Benchmark a
\end{code}
Initially, one would want sequential composition, which we can inherit
from the monad.

We would expect that |>>| in the benchmark domain maps to |+| in the
runtime domain, i.e.,

|seq_runtime_prop b1 b2 = runtime (b1 >> b2) == runtime b1 + runtime b2|

However, clocks are not precise, and the computer may also experience
some external load, therefore this will never be exactly true,
so it should be relaxed to some $\epsilon$.
\begin{code}
(==~)  ::  (Num a, Ord a) => 
           a -> a -> Bool
(==~)  =  undefined
\end{code}
|seq_approx_runtime_prop b1 b2 = 
  runtime (b1 >> b2) ==~ (runtime b1 + runtime b2)|

\begin{code}
type Lock = MVar ()
type Memory = UVM.IOVector Int

memory_access :: Memory -> Benchmark ()
memory_access mem = 
  forM_ [0 .. UVM.length mem] $ \i -> liftIO $ do
    x <- UVM.unsafeRead mem i
    UVM.unsafeWrite mem i ((x+i)*i)
\end{code}
\begin{code}
(|||) :: Benchmark () -> Benchmark () -> Benchmark ()
Benchmark b1 ||| Benchmark b2 = Benchmark $ liftIO $ do
  barrier <- newEmptyMVar
  _ <- forkIO (b1 >> putMVar barrier ())
  b2
  takeMVar barrier
\end{code}


\begin{code}
-- data B mem res where
--     MkB      :: IO mem      -> (mem -> IO res) -> B mem res
--     ShareMem :: B mem res1  -> B mem res2      -> B mem (res1, res2)
--     SepMem   :: B mem1 res1 -> B mem2 res2     -> B (mem1, mem2) (res1, res2)
--     MergeMem :: B (mem, mem) (res1, res2)      -> B mem (res1, res2)

-- setup :: B mem res -> IO mem
-- setup (MkB s _)         = s
-- setup (ShareMem b1 _b2) = setup b1 -- Note this is left-biased.
-- setup (SepMem b1 b2)    = (,) <$> setup b1 <*> setup b2
-- setup (MergeMem b)      = 
--     fst <$> setup b -- The effects from both will still be executed
--                     -- this may or may not be desired (probably not).

-- action :: B mem res -> mem -> IO res
-- action (MkB _m a) = a
-- action (ShareMem b1 b2) =
--     \ arg -> (,) <$> action b1 arg <*> action b2 arg
-- action (SepMem b1 b2) =
--     \ (arg1, arg2) -> (,) <$> action b1 arg1 <*> action b2 arg2
-- action (MergeMem b) = \ arg -> action b (arg, arg)

data B' mem res where
    MkB' :: Mem mem -> Exec mem res -> B' mem res

data Mem mem where
    MkMem    :: IO mem -> Mem mem
    ShareMem :: Mem mem -> Mem mem -> Mem mem
    SepMem   :: Mem mem1 -> Mem mem2 -> Mem (mem1, mem2)
    MergeMem :: Mem (mem, mem) -> Mem mem

setup :: Mem mem -> IO mem
setup (MkMem m) = m
setup (ShareMem m1 _m2) = setup m1 -- Left biased
setup (SepMem m1 m2) = (,) <$> setup m1 <*> setup m2
setup (MergeMem m) = fst <$> setup m

data Exec mem res where
    MkExec :: (mem -> IO res) -> Exec mem res
    ExecPar :: Exec mem1 res1 -> Exec mem2 res2 -> Exec (mem1, mem2) (res1, res2)
    ExecSeq :: Exec mem1 res1 -> Exec mem2 res2 -> Exec (mem1, mem2) (res1, res2)

action :: Exec mem res -> mem -> IO res
action (MkExec f) = f
action (ExecPar e1 e2) = 
    \ (arg1, arg2) -> do
      barrier <- newEmptyMVar
      _ <- forkIO (action e1 arg1 >>= \ !v -> putMVar barrier v)
      res2 <- action e2 arg2
      res1 <- takeMVar barrier
      return (res1, res2)
action (ExecSeq e1 e2) =
    \ (arg1, arg2) -> (,) <$> action e1 arg1
                          <*> action e2 arg2


bench :: B' mem res -> IO (Double, res)
bench (MkB' mem exec) = do
  x <- setup mem
  timeAction (action exec x)

timeAction :: IO a -> IO (Double, a)
timeAction act = do
  t1 <- getCurrentTime
  !r <- act
  t2 <- getCurrentTime
  let diff = fromRational $ toRational (t2 `diffUTCTime` t1)
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
compileBench BenFib  = fibM 30
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
    arbitrary = sized benchGen
    shrink = shrinkBench

shrinkBench (BenPar a b) = [a, b] ++ do
  a' <- shrink a
  b' <- shrink b
  return (BenPar a' b')
shrinkBench (BenSeq a b) =  [a, b] ++ do
  a' <- shrink a
  b' <- shrink b
  return (BenSeq a' b')
shrinkBench (BenLock1 _ b) = [b]
shrinkBench (BenLock2 _ b) = [b]
shrinkBench _a           = []

data BenchSel = BenchSelPar | BenchSelSeq | BenchSelLock1| BenchSelLock2 deriving (Bounded, Enum)


benchGen 0 = return BenFib
benchGen 1 = return BenFib
benchGen n = do
  switch :: Int <- arbitrary
  let op = if switch `rem` 2 == 0 then BenSeq else BenPar 
      l = (n `div` 2) + n `rem` 2
      r = (n `div` 2)
  op <$> benchGen l <*> benchGen r

benchGenPar lk1 lk2 parLimit n = snd <$> benchGenPar' lk1 lk2 parLimit n

benchGenPar' _ _ _ 0 = return (0, BenFib)
benchGenPar' _ _ _ 1 = return (0, BenFib)
benchGenPar' lk1Mb lk2Mb parLimit n = do
  sel <- arbitraryBoundedEnum
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
                (numPar, b) <- benchGenPar' Nothing lk2Mb parLimit (n-1)
                return (numPar, BenLock1 lk1 b)
          Nothing -> benchGenPar' lk1Mb lk2Mb parLimit n
    BenchSelLock2 -> 
        case lk2Mb of
          Just lk2 -> 
              do
                (numPar, b) <- benchGenPar' lk1Mb Nothing parLimit (n-1)
                return (numPar, BenLock2 lk2 b)
          Nothing -> benchGenPar' lk1Mb lk2Mb parLimit n

{- # NOINLINE timeEstimation #-}
timeEstimation :: Bench -> IO Double
timeEstimation (BenPar a b)  = max <$> timeEstimation a <*> timeEstimation b
timeEstimation (BenSeq a b)  = (+) <$> timeEstimation a <*> timeEstimation b
timeEstimation (BenLock1 lk1 b) = timeEstimation b
timeEstimation (BenLock2 lk2 b) = timeEstimation b
timeEstimation b@BenFib = timeBench b
\end{code}

Random checking of tests
\begin{code}
estimate :: Bool -> Bench -> IO Bool
estimate verbose b = do
  estim <- runtimes (timeEstimation b)
  real  <- runtimes (timeBench b)
  let res = threshold estim real -- tTest 0.01 estim real
  when (res || verbose) $ do
    putStrLn (show (Stats.mean estim, Stats.mean real, estim, real))
  return res
          
    where
      numRuns :: Int
      numRuns = 10

      toUnboxed :: UV.Unbox a => V.Vector a -> UV.Vector a
      toUnboxed = UV.fromList . V.toList

      {--- # NOINLINE runtimes #-}
      runtimes :: IO Double -> IO (UV.Vector Double)
      runtimes act = toUnboxed <$> V.tail <$> Traverse.sequenceA (V.replicate (numRuns + 1) act)

threshold :: UV.Vector Double -> UV.Vector Double -> Bool
threshold a b = 
    let
        x1 = filterMean a
        x2 = filterMean b

        filterMean = Stats.mean . setOp (Set.deleteMin . Set.deleteMax)
        setOp f = UV.fromList . Set.toList . f . Set.fromList . UV.toList
    in 
      abs (max x1 x2 / min x1 x2) > 1.10

numPar (BenPar b1 b2) = 1 + numPar b1 + numPar b2
numPar (BenSeq b1 b2) = numPar b1 + numPar b2
numPar _ = 0

-- prop_withoutPar :: Bench -> Property
prop_withoutPar lk1 lk2 n = forAllShrink (sized (benchGenPar lk1 lk2 n)) shrinkBench prop_estimation

prop_estimation :: Bench -> Property
prop_estimation = 
    \ b -> monadicIO $ do
             r <- run $ estimate False b
             assert (not r)


-- runTests :: IO Bool
-- runTests = $quickCheckAll

main :: IO ()
main = do
  lk1 <- newMVar ()
  lk2 <- newMVar ()
  quickCheck (prop_withoutPar (Just lk1) (Just lk2) 5)
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
