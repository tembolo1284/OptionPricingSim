module MonteCarlo (monteCarloOption) where

import RandomWalk (simulatePath)
import Control.Monad (replicateM)
import Types

-- Function to calculate option payoff
optionPayoff :: OptionType -> StrikePrice -> Double -> Double
optionPayoff Call strike finalPrice = max (finalPrice - strike) 0
optionPayoff Put strike finalPrice  = max (strike - finalPrice) 0

-- Monte Carlo option pricing function
monteCarloOption :: InitialStockPrice -> StrikePrice -> InterestRate -> Volatility -> TimeToMaturity -> NumSimulations -> OptionType -> IO Double
monteCarloOption initialStock strike interest vol time numSims optType = do
    -- Simulate multiple paths
    finalPrices <- replicateM numSims (simulatePath initialStock interest vol time)
    -- Calculate payoffs for each path
    let payoffs = map (optionPayoff optType strike) finalPrices
    -- Average the payoffs and discount to present value
    let averagePayoff = sum payoffs / fromIntegral numSims
    return (exp (-interest * time) * averagePayoff)

