module RandomWalk (simulatePath) where

import System.Random (randomRs, newStdGen)
import Types

simulatePath :: InitialStockPrice -> InterestRate -> Volatility -> TimeToMaturity -> IO Double
simulatePath initialStock interest vol time = undefined  -- Implementation details

