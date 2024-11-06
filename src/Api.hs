{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Api where

import Servant
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data OptionRequest = OptionRequest
    { initialStock :: Double
    , strikePrice :: Double
    , interestRate :: Double
    , volatility :: Double
    , timeToMaturity :: Double
    , numSimulations :: Int
    , optionType :: String
    } deriving (Generic, Show)

data OptionResponse = OptionResponse
    { price :: Double
    } deriving (Generic, Show)

instance ToJSON OptionRequest
instance ToJSON OptionResponse
instance FromJSON OptionRequest

type OptionApi = "price" :> ReqBody '[JSON] OptionRequest :> Post '[JSON] OptionResponse
