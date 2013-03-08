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

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad

import           Data.Time.Clock
import qualified Data.Vector.Unboxed.Mutable as V

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
type Memory = V.IOVector Int

exclusion :: Lock -> BenchmarkT a
exclusion l task = lock l *> task <* unlock l

(+<) :: BenchmarkT a -> BenchmarkT a -> BenchmarkT a
(+<) = flip (.)

memory_access :: Memory -> Benchmark ()
memory_access mem = 
  forM_ [0 .. V.length mem] $ \i -> liftIO $ do
    x <- V.unsafeRead mem i
    V.unsafeWrite mem i ((x+i)*i)

lock    :: Lock -> Benchmark ()
lock    = void . liftIO . takeMVar

unlock  :: Lock -> Benchmark ()
unlock  = void . liftIO . flip putMVar ()  
\end{code}
From these we shoudl be able to start constructing a more
complicated version of the basic mutual exclusion, for example:
\begin{code}
complicated_exclusion :: Benchmark () -> Lock -> Benchmark ()
complicated_exclusion memTask lk = do
  exclusion lk memTask
  memTask
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
main2 :: IO ()
main2 = do
  [size] <- getArgs
  mem <- V.replicate (read size) (0 :: Int)
  [lk1, lk2, lk3] <- forM [(1::Int) .. 3] (const (newMVar ()))
  let excl = complicated_exclusion (memory_access mem)
  runtime (excl lk1 ||| excl lk2 ||| excl lk3) >>= print
  -- V.unsafeRead mem (read size - 1) >>= print
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
    BenAtom :: IO () -> Bench
    BenSeq  :: !Bench -> !Bench -> Bench
    BenPar  :: !Bench -> !Bench -> Bench

instance Show Bench where
    show (BenAtom f) = "<atom>"
    show (BenSeq b1 b2) = concat ["(", show b1, ") ; (", show b2,")"]
    show (BenPar b1 b2) = concat ["(", show b1, ") ||| (", show b2,")"]

fib :: Int -> Int
fib n | n > 1     = fib (n-1) + fib (n-2)
      | otherwise = 1
                    
{-# NOINLINE fibM #-}
fibM n =
  let !x = fib n
  in return ()

{-# NOINLINE compileBench #-}
compileBench :: Bench -> IO ()
compileBench (BenAtom act)  = fibM 37
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
    arbitrary = sized benchSeq
        where benchSeq 0 = return test
              benchSeq n = do
                switch :: Int <- arbitrary
                let op = if switch `rem` 2 == 0 then BenSeq else BenPar 
                op <$> (benchSeq (n-1)) <*> benchSeq (n-1)
              test = BenAtom (fibM 37)
    shrink (BenPar a b) = [a,b]
    shrink (BenSeq a b) = [a,b]
    shrink a            = []

{-# NOINLINE timeEstimation #-}
timeEstimation :: Bench -> IO Double
timeEstimation (BenPar a b)  = max <$> timeBench a <*> timeBench b
timeEstimation (BenSeq a b)  = (+) <$> timeBench a <*> timeBench b
timeEstimation b@(BenAtom act) = timeBench b
\end{code}

Random checking of tests
\begin{code}

prop_estimation :: Bench -> Property
prop_estimation = 
    \ b -> monadicIO $ do
        estim <- average (timeEstimation b)
        real  <- average (timeBench b)
        let res = threshold estim real
        when (not res) $ do
          run (putStrLn (show (estim,real)))
          assert res
          
    where
      -- FIXME: this thresholding should probably be replaced with 
      -- something statistically sound. One candidate would be to use
      -- the sample std deviation as a bound instead of a constant 10%.
      threshold a b = abs (b - a)/a < 0.10

      numRuns :: Int
      numRuns = 20
                
      {-# NOINLINE average #-}
      average act = 
        do times <- run (sequence $ replicate numRuns act)
           return (sum times / fromIntegral numRuns)
                             


runTests = $quickCheckAll

main = runTests  
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
