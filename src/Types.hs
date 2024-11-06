{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

-- Type aliases for better readability
type StrikePrice = Double
type InterestRate = Double
type Volatility = Double
type TimeToMaturity = Double
type InitialStockPrice = Double
type NumSimulations = Int
type NumSteps = Int

-- Option type to indicate Call or Put
data OptionType = Call | Put deriving (Show, Generic)
instance ToJSON OptionType
instance FromJSON OptionType

-- Option data to represent an option
data Option = European OptionType StrikePrice TimeToMaturity deriving (Show, Generic)
instance ToJSON Option
instance FromJSON Option

-- Define the OptionRequest data structure
data OptionRequest = OptionRequest
    { initialStock :: InitialStockPrice
    , strikePrice :: StrikePrice
    , interestRate :: InterestRate
    , volatility :: Volatility
    , timeToMaturity :: TimeToMaturity
    , numSimulations :: NumSimulations
    , optionType :: String
    } deriving (Show, Generic)

instance ToJSON OptionRequest
instance FromJSON OptionRequest

