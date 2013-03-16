{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Chiz where

import           Control.Applicative
import           Control.Lens (makeLenses)
import           Control.Monad

import           Data.Aeson as Aeson
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Text as Text

import           GHC.Generics

import qualified Statistics.Resampling.Bootstrap as Stats

import Bench
import Dsl
import DslParse

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
           , "delcs" .= decls
           , "reset" .= reset
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
              <*> ((fmap Map.fromList) <$> (v .:? "opResults"))
  parseJSON _ = mzero

instance ToJSON Operation where
  toJSON (Operation name code resultsMb) =
    object [ "opName" .= name
           , "opCode" .= code
           , "opResults" .= fmap Map.toList  resultsMb
           ]

instance FromJSON BenchDsl where
  parseJSON (String txt) = dslP
    where
      dslP = 
        case parseDsl (Text.unpack txt) of
          Left err -> fail (show err)
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