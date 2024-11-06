module PricingMethods.FiniteDifference (crankNicolsonOption) where

import Data.Vector (Vector, (!), fromList, replicate, (//))
import qualified Data.Vector as V
import Types

-- Crank-Nicolson parameters and constants
type AssetGrid = Vector Double
type TimeGrid = Vector Double

-- Create grids for asset prices and time
createGrids :: InitialStockPrice -> StrikePrice -> Volatility -> TimeToMaturity -> NumSteps -> (AssetGrid, TimeGrid)
createGrids s k v t steps =
    let dS = 2 * k / fromIntegral steps
        dT = t / fromIntegral steps
        assetGrid = fromList [0, dS .. 2 * k]
        timeGrid = fromList [0, dT .. t]
    in (assetGrid, timeGrid)

-- Payoff function at maturity
payoff :: OptionType -> StrikePrice -> AssetGrid -> Vector Double
payoff Call k grid = V.map (\s -> max (s - k) 0) grid
payoff Put k grid = V.map (\s -> max (k - s) 0) grid

-- Crank-Nicolson finite difference solver for a European option
crankNicolsonOption :: InitialStockPrice -> StrikePrice -> InterestRate -> Volatility -> TimeToMaturity -> NumSteps -> OptionType -> Double
crankNicolsonOption s k r v t steps optType =
    let (assetGrid, timeGrid) = createGrids s k v t steps
        dS = assetGrid ! 1 - assetGrid ! 0
        dT = timeGrid ! 1 - timeGrid ! 0
        -- Define alpha, beta, and gamma with scalar values
        alpha = V.map (\s -> 0.25 * dT * ((v * v * s * s / (dS * dS)) - (r / dS))) assetGrid
        beta  = V.map (\s -> -0.5 * dT * (v * v * s * s / (dS * dS) + r)) assetGrid
        gamma = V.map (\s -> 0.25 * dT * ((v * v * s * s / (dS * dS)) + (r / dS))) assetGrid

        -- Initialize the option value at maturity
        optionValue = payoff optType k assetGrid

        -- Time-stepping for Crank-Nicolson
        finalOptionValue = V.foldl' (\uPrev _ -> crankNicolsonStep uPrev alpha beta gamma) optionValue (V.init timeGrid)
    in finalOptionValue ! (steps `div` 2)  -- Retrieve the initial price value for the given strike

-- Crank-Nicolson time-stepping function
crankNicolsonStep :: Vector Double -> Vector Double -> Vector Double -> Vector Double -> Vector Double
crankNicolsonStep uPrev alpha beta gamma = undefined  -- Implement tridiagonal solver here

