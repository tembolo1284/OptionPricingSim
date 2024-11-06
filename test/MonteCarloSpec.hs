-- test/MonteCarloSpec.hs
module MonteCarloSpec where

import Test.Hspec
import MonteCarlo (monteCarloOption)
import PricingMethods.BlackScholes (blackScholesOption)
import Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Monte Carlo Option Pricing" $ do
    it "approximates the Black-Scholes price for large simulations" $ do
        let initialStock = 100
            strikePrice = 100
            interestRate = 0.05
            volatility = 0.2
            timeToMaturity = 1.0
            numSimulations = 100000  -- Large number for better accuracy
            optionType = Call
        mcPrice <- monteCarloOption initialStock strikePrice interestRate volatility timeToMaturity numSimulations optionType
        let bsPrice = blackScholesOption initialStock strikePrice interestRate volatility timeToMaturity optionType
        mcPrice `shouldSatisfy` (\p -> abs (p - bsPrice) < 1.0)  -- Allowable margin of error

