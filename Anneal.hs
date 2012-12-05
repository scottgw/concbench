{-# LANGUAGE ScopedTypeVariables, TupleSections, BangPatterns #-}
module Main (main, genInput) where


import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

import Data.Serialize

import Data.Array
import Data.Array.MArray
import Data.Array.IO
import qualified Data.ByteString as BS
import Data.List
import Data.Vector.Cereal
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as V
import Data.Vector.Unboxed (Vector)

import System.Random
import System.Environment


newCityMap n maxDist = do
  cityMapM <- V.new (n*n)
  
  let loop i
        | i == n*n = return ()
        | otherwise = randomRIO (1,maxDist) >>= V.write cityMapM i >> loop (i+1)
  loop 0
  cityMap <- V.unsafeFreeze cityMapM
  return (CityMap n cityMap)

inputFile = "anneal.in"

genInput n maxDist = do
  cityMap <- newCityMap n maxDist
  BS.writeFile inputFile (encode cityMap)

readInput :: IO (Either String CityMap)
readInput = decode `fmap` BS.readFile inputFile

main = do
  arg:_ <- getArgs
  let n = read arg
  state <- generateState n
  printState state
  mapConcurrently (threadLoop state) [1..n]
  -- threadLoop state
  printState state
  return ()

printState state = do
  route <- map adjCity `fmap` (atomically $ getElems (stRoute state))
  let
    dist !acc prev [] = acc
    dist !acc prev (next:nexts) = 
      dist (acc + interCityDist (stCityMap state) prev next) next nexts
  print (dist 0 (head route) (tail route))
  print (take 20 $ map cityId route)

-- Per thread work
threadLoop :: State -> -- ^ Common state
              Int   -> -- ^ Random seed
              IO ()
threadLoop state seed = loop 0 (-1) initialTemp 0 (mkStdGen seed)
  where 
    loop good bad t i gen = do
      continue <- keepGoing state i good bad
      when continue $ do
        (gen', good', bad') <- threadInnerLoop state t 0 0 numInnerIterations gen
        barrierWait state
        loop good' bad' (cool t) (i + 1) gen'


threadInnerLoop :: State  ->     -- ^ Common state
                   Double ->     -- ^ Temperature 't'
                   Int    ->     -- ^ Number of good decisions
                   Int    ->     -- ^ Number of bad decisions
                   Int    ->     -- ^ Iteration countdown
                   StdGen ->     -- ^ RNG
                   IO (StdGen, Int, Int) -- ^ Final good and bad counts
threadInnerLoop state t good bad 0 gen = return (gen, good, bad)
threadInnerLoop state t good bad i gen = do
  let route = stRoute state
  (gen', a) <- getRandomCity gen route
  (gen'', b) <- getRandomCity gen' route
  
  let delta = calcDelta (stCityMap state) a b
  (gen''', decision) <- decide gen'' state delta t
  -- print (delta, decision) 
  let bump Good = (good + 1, bad)
      bump Bad  = (good, bad + 1)
  
  case decision of
    Accept goodOrBad -> do
      let (good', bad') = bump goodOrBad
      swapCities state (adjRouteIdx a) (adjRouteIdx b)
      threadInnerLoop state t good' bad' (i-1) gen'''
    Deny        -> threadInnerLoop state t good bad (i-1) gen'''
 
numInnerIterations = 40

initialTemp :: Double
initialTemp = 500.0

cool :: Double -> Double
cool t = t / 1.5


-- State information 
data State = 
  State { stCityMap    :: CityMap
        , stBarrier    :: Barrier
        , stNumWorkers :: Int
        , stStop       :: TVar Bool
        , stRoute      :: Route
        }

generateState numWorkers = do
  Right (cityMap@(CityMap numCities _)) <- readInput

  let cities = map mkCity nums
      nums   = [0..numCities - 1]

  stop <- newTVarIO False
  count <- newTVarIO 0
  done <- newTVarIO 0

  route <- atomically $ do
    let mkAdjCity i = AdjCity Nothing Nothing i (mkCity i)
    route <- newArray_ (0, numCities - 1)
    mapM_ (\i -> writeArray route i (mkAdjCity i)) nums
    mapM_ (\i -> update route i (mkCity i)) nums
    return route

  return $ State { stCityMap    = cityMap
                 , stBarrier    = Barrier count done
                 , stNumWorkers = numWorkers
                 , stStop       = stop
                 , stRoute      = route
                 }
-- -- Random operations
-- stateRandom :: Random a =>  -> IO a
-- stateRandom state = atomically $ do
--   !gen <- readTVar (stGen state)
--   let (!v, !gen') = random gen
--   writeTVar (stGen state) gen'
--   return v

-- stateRandomR :: Random a => State -> (a, a) -> IO a
-- stateRandomR state range = atomically $ do
--   !gen <- readTVar (stGen state)
--   let (!v, !gen') = randomR range  gen
--   writeTVar (stGen state) gen'
--   return v
  

--  
-- City and map related functions and data
--
  
adjCityId = cityId . adjCity
mkCity i = City i (show i)
type Route = TArray Int AdjCity

data AdjCity = AdjCity { adjPrev :: Maybe City
                       , adjNext :: Maybe City
                       , adjRouteIdx :: Int
                       , adjCity     :: City
                       } deriving Show

data City = City { cityId :: Int
                 , cityName :: String
                 } deriving Show

data CityMap = CityMap !Int !(Vector CityDist) deriving (Read, Show)

instance Serialize CityMap where
  get = do
    !n <- get
    !v <- get
    return (CityMap n v)
  
  put (CityMap n v) = do
    put n
    put v

type CityDist = Double

interCityDist :: CityMap -> City -> City -> CityDist
interCityDist (CityMap n cityMap) a b = cityMap V.! (cityId a * n + cityId b)

--
-- Thread helper functions
--

getRandomCity :: StdGen -> Route -> IO (StdGen, AdjCity)
getRandomCity gen route = do
  (l,u) <- atomically $ getBounds route
  let (i, gen') = randomR (l, u) gen
  c <- atomically $ readArray route i
  return (gen', c)

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

data Barrier = Barrier { barrCount :: TVar Int
                       , barrDone  :: TVar Int
                       }


update :: Route -> Int -> City -> STM ()
update route old city = do
  (l, u) <- getBounds route
  prev <- if old - 1 < l
    then return Nothing
    else do
      left <- readArray route (old - 1)
      writeArray route (old - 1) (left {adjNext = Just city})
      return (Just $ adjCity left)
  next <- if old + 1 > u
    then return Nothing
    else do
      right <- readArray route (old + 1)
      writeArray route (old + 1) (right {adjPrev = Just city})
      return (Just $ adjCity right)
  writeArray route old (AdjCity prev next old city)

noDoubles :: Route -> String -> STM ()
noDoubles route str = do
  es <- getElems route
  let es' = map adjCityId es
      inv = sort (nub es') == sort es'
  when (not inv) (do
    es <- map adjCityId `fmap` getElems route
    error $ "swapcities: doubles " ++ show es' ++ " -- " ++ str)


-- | Change cities, update the cities on either side of the swapped cities
-- to indicate their new adjacent neighbour.
-- swapCities :: State -> Int -> Int -> IO [Int]
swapCities state a b = atomically $ do
  let route = stRoute state
  -- noDoubles route "start"
  aCity <- adjCity `fmap` readArray route a
  bCity <- adjCity `fmap` readArray route b
  update route b aCity
  update route a bCity
  -- noDoubles route ("end " ++ show a ++ " with " ++ show b)
  
decide :: StdGen -> State -> CityDist -> Double -> IO (StdGen, Accept)
decide gen state deltaDist t 
  | deltaDist < 0 = return (gen, Accept Good)
  | otherwise = do
      let 
        (rand, gen') = random gen
        boltzman = exp (- deltaDist / t)
      if boltzman > rand
        then return (gen', Accept Bad)
        else return (gen', Deny)

keepGoing :: State -> Int -> Int -> Int -> IO Bool
keepGoing state i good bad = atomically $ do 
    stop <- readTVar (stStop state)
    let cond = not stop && good > bad
    when (not cond) (writeTVar (stStop state) True)
    return cond

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
