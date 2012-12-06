{-# LANGUAGE BangPatterns #-}
module City where

import Control.Concurrent.STM

import Data.Binary

import Data.Vector.Binary ()
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as V
import Data.Vector.Unboxed (Vector)

import System.Random

--
-- City map generation and data definition
--

newCityMap :: Int -> Double -> IO CityMap
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

inputFile :: String
inputFile = "anneal.in"

genInput :: Int -> Double -> IO ()
genInput n maxDist = do
  cityMap <- newCityMap n maxDist
  encodeFile inputFile cityMap

readInput :: IO CityMap
readInput = decodeFile inputFile


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


--  
-- Route and adjacency functions and data
--
type Route = TArray Int City

data AdjCity = AdjCity { adjPrev :: Maybe City
                       , adjNext :: Maybe City
                       , adjRouteIdx :: Int
                       , adjCity     :: City
                       } deriving Show

data City = City { cityId :: Int
                 , cityName :: String
                 } deriving Show

adjCityId :: AdjCity -> Int
adjCityId = cityId . adjCity

mkCity :: Int -> City
mkCity i = City i (show i)

interCityDist :: CityMap -> City -> City -> CityDist
interCityDist (CityMap n cityMap) a b = cityMap V.! (cityId a * n + cityId b)
