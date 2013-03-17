{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Chiz where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens (view, makeLenses)
import           Control.Monad

import           Data.Aeson as Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8 as BS
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as V

import           GHC.Generics

import qualified Statistics.Resampling.Bootstrap as Stats

import           Text.CSV

import           Dsl
import           DslParse

deriving instance Read Stats.Estimate

-- The following two give us an instance for the Estimate
deriving instance Generic Stats.Estimate
instance FromJSON Stats.Estimate
instance ToJSON Stats.Estimate

data ChizInput = 
  ChizInput
  { _chizTestName :: String
  , _chizTestCases :: [BenchDsl]
  , _chizTestElements :: [Element]
  } deriving Show

instance FromJSON ChizInput where
  parseJSON (Object v) = 
    ChizInput <$> v .: "name" 
              <*> v .: "cases"
              <*> v .: "elements"
  parseJSON _ = mzero

instance ToJSON ChizInput where
  toJSON (ChizInput name cases elems) =
    object [ "name" .= name
           , "cases" .= cases
           , "elements" .= elems
           ]

data Element =
  Element 
  { _elementType :: String
  , _elementName :: String
  , _elementDecls :: Set String
  , _elementReset :: String
  , _elementOperations :: [Operation]
  } deriving Show

instance FromJSON Element where
  parseJSON (Object v) =
    Element <$> v .: "type"
            <*> v .: "name"
            <*> v .: "decls"
            <*> (unlines <$> v .: "reset")
            <*> v .: "operations"
  parseJSON _ = mzero

instance ToJSON Element where
  toJSON (Element typee name decls reset ops) =
    object [ "type" .= typee
           , "name" .= name
           , "decls" .= decls
           , "reset" .= lines reset
           , "operations" .= ops
           ]
  
data Operation = 
  Operation 
  { _opName :: String
  , _opCode :: String
  , _opResults :: Maybe BenchMap
  } deriving Show

instance FromJSON Operation where
  parseJSON (Object v) =
    Operation <$> v .: "opName"
              <*> v .: "opCode"
              <*> parseBenchMap
    where
      parseBenchMap :: Parser (Maybe BenchMap)
      parseBenchMap =
        do bmMb <- v .:? "opResults"
           case bmMb of
             Nothing -> return Nothing
             Just bm -> return (Just $ Map.fromList bm)
  parseJSON _ = mzero

instance ToJSON Operation where
  toJSON (Operation name code resultsMb) =
    object [ "opName" .= name
           , "opCode" .= code
           , "opResults" .= fmap Map.toList resultsMb
           ]

instance FromJSON BenchDsl where
  parseJSON (String txt) = dslP
    where
      dslP = 
        case parseDsl (Text.unpack txt) of
          Left err -> error (show txt) -- fail (show err)
          Right v -> return v
  parseJSON _ = mzero

instance ToJSON BenchDsl where
  toJSON dsl = String (Text.pack $ pretty dsl)

-- | The characterization description and results.
data Chiz =
  Chiz 
  { charType :: String
  , charImpl :: String
  , charOperation   :: String
  , charRepl :: String
  , charResults :: Maybe BenchMap
  } deriving (Read, Show)

type BenchMap = Map BenchDsl Stats.Estimate


makeLenses ''ChizInput
makeLenses ''Element
makeLenses ''Operation


-- | Conversion of the Chiz to CSV to consume by
-- other tools such as R (for charts).
writeChizCSV :: FilePath -> ChizInput -> IO ()
writeChizCSV path chizIn = do
  let header = ["type", "operation", "test", "avg", "lower", "upper"]
  writeFile path (printCSV (header : chizToRecords chizIn))

chizToRecords :: ChizInput -> [Record]
chizToRecords = concatMap elmToRecords . view chizTestElements

elmToRecords :: Element -> [Record]
elmToRecords elm =
  map (elmTypeField:)
      (concatMap opToRecord (view elementOperations elm))               
  where
    elmTypeField = view elementType elm

opToRecord :: Operation -> [Record]
opToRecord op =
    map (opNameField:)
        (resultRecords (view opResults op))
    where
      resultRecords = maybe [] resultsToRecords
      
      opNameField = view opName op

      statFuncs = [Stats.estPoint, Stats.estLowerBound, Stats.estUpperBound]

      resultsToRecords :: BenchMap -> [Record]
      resultsToRecords =
        map (uncurry (:)) .
        map (pretty *** (\e -> map (show . ($ e)) statFuncs)) .
        Map.toList
