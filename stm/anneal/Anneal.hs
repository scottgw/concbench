{-# LANGUAGE ScopedTypeVariables, TupleSections, BangPatterns #-}
module Main (main, genInput) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

import Data.Array.MArray
-- import Data.List

import System.Random (StdGen, random, randomR, mkStdGen)
import System.Environment

import City
import State


main :: IO ()
main = do
  nArg:outerIterArg:innerIterArg:tempArg:_ <- getArgs
  let n = read nArg
      innerIters = read innerIterArg
      outerIters = read outerIterArg
      temp = read tempArg

  state <- generateState n outerIters innerIters temp
  printState state
  mapConcurrently (threadLoop state) [1..n]
  
  -- threadLoop state
  printState state
  return ()

--
-- Thread worker outer loop
--
  
threadLoop :: State -> -- ^ Common state
              Int   -> -- ^ Random seed
              IO ()
threadLoop state seed = 
  loop 0 (-1) (stInitTemp state) 0 (mkStdGen seed)
  where 
    loop good bad t i gen = do
      continue <- keepGoing state i good bad
      when continue $ do
        (gen', cityA) <- getRandomAdjCity gen (stRoute state)
        (gen'', good', bad') <- threadInnerLoop state cityA t 0 0 (stInnerIters state) gen'
        barrierWait state
        loop good' bad' (cool t) (i + 1) gen''


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
                   StdGen ->     -- ^ RNG
                   IO (StdGen, Int, Int) -- ^ Final good and bad counts
threadInnerLoop _ _ _ good bad 0 gen = return (gen, good, bad)
threadInnerLoop state cityA t good bad i gen = do
  let route = stRoute state
  (gen', cityB) <- getRandomAdjCity gen route
  
  let delta = calcDelta (stCityMap state) cityA cityB
  (gen'', decision) <- decide gen' delta t
  -- print (delta, decision) 
  let bump Good = (good + 1, bad)
      bump Bad  = (good, bad + 1)
  
  case decision of
    Accept goodOrBad -> do
      let (good', bad') = bump goodOrBad
      swapCities state (adjRouteIdx cityA) (adjRouteIdx cityB)
      threadInnerLoop state cityB t good' bad' (i-1) gen''
    Deny        -> threadInnerLoop state cityB t good bad (i-1) gen''
 

--
-- Thread helper functions
--

getRandomAdjCity :: StdGen -> Route -> IO (StdGen, AdjCity)
getRandomAdjCity gen route = do
  (l,u) <- atomically $ getBounds route
  let (i, gen') = randomR (l, u) gen
  c <- atomically $ readArray route i
  prev <- if i <= l
          then return Nothing
          else Just `fmap` (atomically $ readArray route (i-1))
  next <- if i >= u
          then return Nothing
          else Just `fmap` (atomically $ readArray route (i+1))
  return (gen', AdjCity prev next i c)

-- Important property: the before distance should never be InfDist.
-- Reason: There should never be an InfDist in an existing route.
calcDelta :: CityMap -> AdjCity -> AdjCity -> CityDist
calcDelta cityMap a b = distAfter - distBefore
  where
    dist (AdjCity prev post _ city) = 
      maybe 0 (interCityDist cityMap city) prev +
      maybe 0 (interCityDist cityMap city) post  

    swapPos x y = x { adjPrev = adjPrev y
                    , adjNext = adjNext y
                    }

    a' = swapPos a b
    b' = swapPos b a 

    distBefore = dist a + dist b
    distAfter  = dist a' + dist b'

-- Control

data GoodOrBad = Good | Bad deriving Show
data Accept = Accept GoodOrBad | Deny deriving Show

update :: Route -> Int -> City -> STM ()
update route old city = writeArray route old city

-- noDoubles :: Route -> String -> STM ()
-- noDoubles route str = do
--   es <- getElems route
--   let es' = map cityId es
--       inv = sort (nub es') == sort es'
--   when (not inv) (error $ "swapcities: doubles " ++ show es ++ " -- " ++ str)


-- | Change cities, update the cities on either side of the swapped cities
-- to indicate their new adjacent neighbour.
swapCities :: State -> Int -> Int -> IO ()
swapCities state a b = atomically $ do
  let route = stRoute state
  -- noDoubles route "start"
  aCity <- readArray route a
  bCity <- readArray route b
  update route a bCity
  update route b aCity
  -- noDoubles route ("end " ++ show a ++ " with " ++ show b)
  
decide :: StdGen -> CityDist -> Double -> IO (StdGen, Accept)
decide gen deltaDist t 
  | deltaDist < 0 = return (gen, Accept Good)
  | otherwise = do
      let 
        (rand, gen') = random gen
        boltzman = exp (- deltaDist / t)
      if boltzman > rand
        then return (gen', Accept Bad)
        else return (gen', Deny)
