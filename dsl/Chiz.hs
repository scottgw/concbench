{-# LANGUAGE StandaloneDeriving #-}
module Chiz where

import           Data.Map (Map)

import qualified Statistics.Resampling.Bootstrap as Stats

import Bench
import Java

deriving instance Read Stats.Estimate

-- | The characterization description and results.
data Chiz = 
  Chiz 
  { charName :: String
  , charRepl :: String
  , charResults :: Maybe (BenchMap Java)
  } deriving (Read, Show)


type BenchMap a = Map a Stats.Estimate
