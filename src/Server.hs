-- src/Server.hs
module Server (server, app) where  -- Export only server and app

import Api (OptionApi, OptionRequest(..), OptionResponse(..))  -- Ensure OptionResponse is imported correctly
import Servant
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy)

-- Define your API server
server :: Server OptionApi
server = priceHandler

-- Define the application for the server
app :: Application
app = serve (Proxy :: Proxy OptionApi) server

-- Define your handler for processing OptionRequest
priceHandler :: OptionRequest -> Handler OptionResponse
priceHandler req = do
    -- Example placeholder response with a Double
    let response = OptionResponse 42.0  -- Replace 42.0 with actual calculated value as needed
    liftIO $ pure response

