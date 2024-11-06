module Main (main) where

import Test.Hspec
import qualified MonteCarloSpec
import qualified BlackScholesSpec
import qualified FiniteDifferenceSpec
import qualified ServerSpec

main :: IO ()
main = hspec $ do
    describe "Monte Carlo Pricing Tests" MonteCarloSpec.spec
    describe "Black-Scholes Pricing Tests" BlackScholesSpec.spec
    describe "Finite Difference Pricing Tests" FiniteDifferenceSpec.spec
    describe "Server API Tests" ServerSpec.spec

