module RandomWalk (simulatePath) where

import System.Random.MWC (createSystemRandom, GenIO)
import System.Random.MWC.Distributions (standard)
import Control.Monad (replicateM)
import Types

simulatePath :: InitialStockPrice -> InterestRate -> Volatility -> TimeToMaturity -> IO Double
simulatePath s0 r sigma t = do
    gen <- createSystemRandom
    z <- standard gen  -- This generates a standard normal N(0,1)
    let st = s0 * exp ((r - 0.5 * sigma^2) * t + sigma * sqrt t * z)
    return st