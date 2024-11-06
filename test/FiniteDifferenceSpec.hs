-- test/FiniteDifferenceSpec.hs
module FiniteDifferenceSpec where

import Test.Hspec
import PricingMethods.FiniteDifference (crankNicolsonOption)
import PricingMethods.BlackScholes (blackScholesOption)
import Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Crank-Nicolson Option Pricing" $ do
    it "approximates the Black-Scholes price for a small grid" $ do
        let initialStock = 100
            strikePrice = 100
            interestRate = 0.05
            volatility = 0.2
            timeToMaturity = 1.0
            numSteps = 100  -- Small grid
            optionType = Call
        let cnPrice = crankNicolsonOption initialStock strikePrice interestRate volatility timeToMaturity numSteps optionType
        let bsPrice = blackScholesOption initialStock strikePrice interestRate volatility timeToMaturity optionType
        cnPrice `shouldSatisfy` (\p -> abs (p - bsPrice) < 1.0)  -- Allowable margin of error

