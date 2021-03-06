{-# LANGUAGE ScopedTypeVariables, TupleSections, BangPatterns #-}
module Main (main, genInput) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

import Data.Array.MArray

import System.Random.Mersenne -- Random (StdGen, random, randomR, mkStdGen)
import System.Environment
import System.TimeIt

import City
import State


numRuns = 10

main :: IO ()
main = do
  nArg:outerIterArg:innerIterArg:tempArg:_ <- getArgs
  let n = read nArg
      innerIters = read innerIterArg
      outerIters = read outerIterArg
      temp = read tempArg
  cityMap <- readInput
  --   gen <- newMTGen (Just 42)
  -- setStdGen gen
  let test = do
        state <- generateState cityMap n outerIters innerIters temp
        -- printState state
        mapConcurrently (threadLoop state) [1..n]
        -- printState state
        
  (secs, _) <- timeItT (replicateM numRuns test)
  
  putStrLn (show (secs/fromIntegral numRuns) ++ " s")
  
  return ()

--
-- Thread worker outer loop
--
  
threadLoop :: State -> -- ^ Common state
              Int   -> -- ^ Random seed
              IO ()
threadLoop state seed = do
  gen <- getStdGen
  loop 0 (-1) (stInitTemp state) 0 gen
  where 
    loop good bad t i gen = do
      continue <- keepGoing state i good bad
      when continue $ do
        cityA <- getRandomAdjCity gen (stRoute state)
        (good', bad') <- threadInnerLoop state cityA t 0 0 (stInnerIters state) gen
        -- barrierWait state
        loop good' bad' (cool t) (i + 1) gen


-- | A cooling function that decreases the temperature.
cool :: Double -> Double
cool t = t / 1.5


keepGoing :: State -> Int -> Int -> Int -> IO Bool
keepGoing state i good bad = 
  case stMaxSteps state of
    Nothing -> atomically $ do 
      stop <- readTVar (stStop state)
      let cond = not stop && good > bad
      when (not cond) (writeTVar (stStop state) True)
      return cond
    Just maxSteps -> return (i < maxSteps)

-- | Waits at a barrier for 'n' workers to arrive. It will
-- skip the operation of stop has been indicated by the TVar.
barrierWait :: State -> IO ()
barrierWait state = barrierWork
  where
    n = stNumWorkers state

    var = barrCount $ stBarrier state
    doneVar = barrDone $ stBarrier state

    joinBarrier = do
      stop <- readTVar (stStop state) 
      when (not stop) $ do
        done <- readTVar doneVar
        check (done == 0)
        modifyTVar' var (+1)
    
    waitBarrier = do
      stop <- readTVar (stStop state)
      when (not stop) $ do
        x <- readTVar var
        check (x == n)
        modifyTVar'  doneVar (+1)
        done <- readTVar doneVar
        if done == n
          then writeTVar var 0 >> writeTVar doneVar 0
          else return ()
    
    barrierWork = atomically joinBarrier >> atomically waitBarrier


--
-- Thread worker inner loop
--
  
threadInnerLoop :: State  ->     -- ^ Common state
                   AdjCity   ->     -- ^ Last city swapped
                   Double ->     -- ^ Temperature 't'
                   Int    ->     -- ^ Number of good decisions
                   Int    ->     -- ^ Number of bad decisions
                   Int    ->     -- ^ Iteration countdown
                   MTGen ->     -- ^ RNG
                   IO (Int, Int) -- ^ Final good and bad counts
threadInnerLoop _ _ _ !good !bad 0 _ = return (good, bad)
threadInnerLoop state !cityA !t !good !bad !i !gen = do
  let route = stRoute state
  cityB <- getRandomAdjCity gen route
  
  let delta = calcDelta (stCityMap state) cityA cityB
  decision <- decide gen delta t
  -- print (delta, decision) 
  let bump Good = (good + 1, bad)
      bump Bad  = (good, bad + 1)
  
  case decision of
    Accept goodOrBad -> do
      let (good', bad') = bump goodOrBad
      swapCities state (adjRouteIdx cityA) (adjRouteIdx cityB)
      threadInnerLoop state cityB t good' bad' (i-1) gen
    Deny        -> threadInnerLoop state cityB t good bad (i-1) gen
 

--
-- Thread helper functions
--

randomR :: (Int, Int) -> MTGen -> IO Int
randomR (!l, !u) gen = bound <$> random gen
  where
    bound i = (abs i `rem` u - l) + l

-- getRandomAdjCity :: MTGen -> Route -> IO AdjCity
-- getRandomAdjCity gen route = do
--   (l,u) <- atomically $ getBounds route
--   i <- randomR (l, u) gen

--   c <- atomically $ readArray route i
--   let readAdj j = Just `fmap` (atomically $ readArray route j)
--   prev <- if i <= l
--           then return Nothing
--           else readAdj (i-1)
--   next <- if i >= u
--           then return Nothing
--           else readAdj (i+1)
--   return (AdjCity prev next i c)


getRandomAdjCity :: MTGen -> Route -> IO AdjCity
getRandomAdjCity gen route = do
  (l,u) <- atomically $ getBounds route
  i <- randomR (l, u) gen
  atomically $ do
    c <- readArray route i
    let readAdj j = Just <$> readArray route j
    prev <- if i <= l
            then return Nothing
            else readAdj (i-1)
    next <- if i >= u
            then return Nothing
            else readAdj (i+1)
    return (AdjCity prev next i c)


-- Important property: the before distance should never be InfDist.
-- Reason: There should never be an InfDist in an existing route.
calcDelta :: CityMap -> AdjCity -> AdjCity -> CityDist
calcDelta cityMap !a !b = distAfter - distBefore
  where
    dist (AdjCity prev post _ city) = 
      let dist' = maybe 0 (interCityDist cityMap city)
      in dist' prev + dist' post  

    swapPos x y = x { adjPrev = adjPrev y
                    , adjNext = adjNext y
                    }

    !a' = swapPos a b
    !b' = swapPos b a 

    distBefore = dist a + dist b
    distAfter  = dist a' + dist b'

-- Control

data GoodOrBad = Good | Bad deriving Show
data Accept = Accept GoodOrBad | Deny deriving Show

-- | Change cities, update the cities on either side of the swapped cities
-- to indicate their new adjacent neighbour.
swapCities :: State -> Int -> Int -> IO ()
swapCities state !a !b = atomically $ do
  let route = stRoute state
  aCity <- readArray route a
  bCity <- readArray route b
  writeArray route a bCity
  writeArray route b aCity
  
decide :: MTGen -> CityDist -> Double -> IO Accept
decide !gen !deltaDist !t 
  | deltaDist < 0 = return (Accept Good)
  | otherwise = do
      rand <- random gen
      let boltzman = exp (- deltaDist / t)
      if boltzman > rand
        then return (Accept Bad)
        else return Deny
