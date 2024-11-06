module BlackScholesSpec where

import Test.Hspec
import PricingMethods.BlackScholes (blackScholesOption)
import Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Black-Scholes Option Pricing" $ do
    it "calculates the correct price for a known call option" $ do
        let initialStock = 100
            strikePrice = 100
            interestRate = 0.05
            volatility = 0.2
            timeToMaturity = 1.0
            optionType = Call
            expectedPrice = 10.4506
        let bsPrice = blackScholesOption initialStock strikePrice interestRate volatility timeToMaturity optionType
        bsPrice `shouldSatisfy` (\p -> abs (p - expectedPrice) < 0.01)  -- Allowing a tolerance of 0.001
