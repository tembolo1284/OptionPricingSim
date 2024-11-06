{-# LANGUAGE RecordWildCards #-}

module Server (server, app) where

import Api (OptionApi, OptionRequest(..), OptionResponse(..))
import MonteCarlo (monteCarloOption)
import Servant
import Network.Wai (Application)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy)
import Types (OptionType(..))

-- Define your API server
server :: Server OptionApi
server = priceHandler

-- Define the application for the server
app :: Application
app = serve (Proxy :: Proxy OptionApi) server

-- Define your handler for processing OptionRequest
priceHandler :: OptionRequest -> Handler OptionResponse
priceHandler OptionRequest{..} = do
    -- Convert optionType from String to OptionType data type
    let optType = if optionType == "Call" then Call else Put

    -- Calculate the option price using monteCarloOption
    calculatedPrice <- liftIO $ monteCarloOption initialStock strikePrice interestRate volatility timeToMaturity numSimulations optType
    pure $ OptionResponse calculatedPrice
