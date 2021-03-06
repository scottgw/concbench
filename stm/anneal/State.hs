{-# LANGUAGE BangPatterns #-}
module State where

import Control.Concurrent.STM

import Data.Array.MArray

import City

-- State information 
data State = 
  State { stCityMap    :: !CityMap
        , stInitTemp   :: !Double
        , stInnerIters :: !Int
        , stBarrier    :: !Barrier
        , stMaxSteps   :: !(Maybe Int)
        , stNumWorkers :: !Int
        , stStop       :: !(TVar Bool)
        , stRoute      :: !Route
        }

data Barrier = Barrier { barrCount :: !(TVar Int)
                       , barrDone  :: !(TVar Int)
                       }



generateState :: CityMap -> Int -> Int -> Int -> Double -> IO State
generateState cityMap@(CityMap numCities _) numWorkers outerIters innerIters temp = do
  let nums   = [0..numCities - 1]

  stop <- newTVarIO False
  count <- newTVarIO 0
  done <- newTVarIO 0

  route <- atomically $ do
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

printState :: State -> IO ()
printState state = do
  route <- atomically $ getElems (stRoute state)
  let
    dist !acc    _ [] = acc
    dist !acc prev (next:nexts) = 
      dist (acc + interCityDist (stCityMap state) prev next) next nexts
  print (dist 0 (head route) (tail route))
  print (take 20 $ map cityId route)
