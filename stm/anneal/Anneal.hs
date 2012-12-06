{-# LANGUAGE ScopedTypeVariables, TupleSections, BangPatterns #-}
module Main (main, genInput) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

import Data.Binary

import Data.Array.MArray
import qualified Data.ByteString as BS
import Data.List

import Data.Vector.Binary
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as V
import Data.Vector.Unboxed (Vector)

import System.Random
import System.Environment

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



newCityMap n maxDist = do
  cityMapM <- V.new (n*n)
  
  let loop i
        | i == n*n = return ()
        | otherwise = do
            d <- randomRIO (1,maxDist)
            V.write cityMapM i d -- asymmetric distances!
            loop (i+1)
  loop 0
  cityMap <- V.unsafeFreeze cityMapM
  return (CityMap n cityMap)

inputFile = "anneal.in"

genInput n maxDist = do
  cityMap <- newCityMap n maxDist
  encodeFile inputFile cityMap

readInput :: IO CityMap
readInput = decodeFile inputFile

printState state = do
  route <- atomically $ getElems (stRoute state)
  let
    dist !acc prev [] = acc
    dist !acc prev (next:nexts) = 
      dist (acc + interCityDist (stCityMap state) prev next) next nexts
  print (dist 0 (head route) (tail route))
  print (take 20 $ map cityId route)

--
-- Thread worker
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


threadInnerLoop :: State  ->     -- ^ Common state
                   AdjCity   ->     -- ^ Last city swapped
                   Double ->     -- ^ Temperature 't'
                   Int    ->     -- ^ Number of good decisions
                   Int    ->     -- ^ Number of bad decisions
                   Int    ->     -- ^ Iteration countdown
                   StdGen ->     -- ^ RNG
                   IO (StdGen, Int, Int) -- ^ Final good and bad counts
threadInnerLoop state cityA t good bad 0 gen = return (gen, good, bad)
threadInnerLoop state cityA t good bad i gen = do
  let route = stRoute state
  (gen', cityB) <- getRandomAdjCity gen route
  
  let delta = calcDelta (stCityMap state) cityA cityB
  (gen'', decision) <- decide gen' state delta t
  -- print (delta, decision) 
  let bump Good = (good + 1, bad)
      bump Bad  = (good, bad + 1)
  
  case decision of
    Accept goodOrBad -> do
      let (good', bad') = bump goodOrBad
      swapCities state (adjRouteIdx cityA) (adjRouteIdx cityB)
      threadInnerLoop state cityB t good' bad' (i-1) gen''
    Deny        -> threadInnerLoop state cityB t good bad (i-1) gen''
 


cool :: Double -> Double
cool t = t / 1.5


-- State information 
data State = 
  State { stCityMap    :: CityMap
        , stInitTemp   :: Double
        , stInnerIters :: Int
        , stBarrier    :: Barrier
        , stMaxSteps   :: Maybe Int
        , stNumWorkers :: Int
        , stStop       :: TVar Bool
        , stRoute      :: Route
        }

generateState numWorkers outerIters innerIters temp = do
  cityMap@(CityMap numCities _) <- readInput

  let cities = map mkCity nums
      nums   = [0..numCities - 1]

  stop <- newTVarIO False
  count <- newTVarIO 0
  done <- newTVarIO 0

  route <- atomically $ do
    let mkAdjCity i = AdjCity Nothing Nothing i (mkCity i)
    route <- newArray_ (0, numCities - 1)
    mapM_ (\i -> writeArray route i (mkCity i)) nums
    return route

  return $ State { stCityMap    = cityMap
                 , stBarrier    = Barrier count done
                 , stInnerIters = innerIters
                 , stInitTemp   = temp
                 , stMaxSteps   = Just outerIters
                 , stNumWorkers = numWorkers
                 , stStop       = stop
                 , stRoute      = route
                 }  

--  
-- City and map related functions and data
--
  
adjCityId = cityId . adjCity
mkCity i = City i (show i)
type Route = TArray Int City

data AdjCity = AdjCity { adjPrev :: Maybe City
                       , adjNext :: Maybe City
                       , adjRouteIdx :: Int
                       , adjCity     :: City
                       } deriving Show

data City = City { cityId :: Int
                 , cityName :: String
                 } deriving Show

data CityMap = CityMap !Int !(Vector CityDist) deriving (Read, Show)

instance Binary CityMap where
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

data Barrier = Barrier { barrCount :: TVar Int
                       , barrDone  :: TVar Int
                       }


update :: Route -> Int -> City -> STM ()
update route old city = writeArray route old city

noDoubles :: Route -> String -> STM ()
noDoubles route str = do
  es <- getElems route
  let es' = map cityId es
      inv = sort (nub es') == sort es'
  when (not inv) (do
    es <- getElems route
    error $ "swapcities: doubles " ++ show es ++ " -- " ++ str)


-- | Change cities, update the cities on either side of the swapped cities
-- to indicate their new adjacent neighbour.
-- swapCities :: State -> Int -> Int -> IO [Int]
swapCities state a b = atomically $ do
  let route = stRoute state
  -- noDoubles route "start"
  aCity <- readArray route a
  bCity <- readArray route b
  update route a bCity
  update route b aCity
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
