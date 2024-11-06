module ServerSpec where

import Test.Hspec
import Test.Hspec.Wai
import Network.Wai (Application)
import Server (app)
import Api (OptionRequest(..))
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI  -- Import CaseInsensitive

-- Set up the application for testing
withApp :: IO Application
withApp = return app

main :: IO ()
main = hspec spec

spec :: Spec
spec = with withApp $ do
    describe "POST /price" $ do
        it "returns the correct response format for a Monte Carlo request" $ do
            let requestBody = encode $ OptionRequest {
                    initialStock = 100,
                    strikePrice = 100,
                    interestRate = 0.05,
                    volatility = 0.2,
                    timeToMaturity = 1.0,
                    numSimulations = 10000,
                    optionType = "MonteCarlo"
                }
            request (BS.pack "POST") (BS.pack "/price") [(CI.mk (BS.pack "Content-Type"), BS.pack "application/json")] requestBody
                `shouldRespondWith` 200
