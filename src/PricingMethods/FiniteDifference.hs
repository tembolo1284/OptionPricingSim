module PricingMethods.FiniteDifference (crankNicolsonOption) where

import Data.Vector (Vector, (!), fromList, (//))
import qualified Data.Vector as V
import Types
import Control.Exception (assert)

type AssetGrid = Vector Double
type TimeGrid = Vector Double

createGrids :: InitialStockPrice -> StrikePrice -> Volatility -> TimeToMaturity -> NumSteps -> (AssetGrid, TimeGrid)
createGrids s k v t steps =
    let sMax = 2 * max s k  -- Ensure grid covers enough space
        dS = sMax / fromIntegral steps
        dT = t / fromIntegral steps
        assetGrid = V.generate (steps + 1) (\i -> fromIntegral i * dS)
        timeGrid = V.generate (steps + 1) (\i -> fromIntegral i * dT)
    in (assetGrid, timeGrid)

payoff :: OptionType -> StrikePrice -> AssetGrid -> Vector Double
payoff optType k grid = V.map calcPayoff grid
  where
    calcPayoff s = case optType of
        Call -> max (s - k) 0
        Put  -> max (k - s) 0

-- Simplified Crank-Nicolson implementation with stability checks
crankNicolsonOption :: InitialStockPrice -> StrikePrice -> InterestRate -> Volatility -> TimeToMaturity -> NumSteps -> OptionType -> Double
crankNicolsonOption s k r v t steps optType =
    assert (steps > 0 && steps <= 1000) $  -- Reasonable bounds for grid size
    assert (t > 0 && t <= 10) $           -- Reasonable bounds for time
    let 
        -- Create grids
        (assetGrid, timeGrid) = createGrids s k v t steps
        dS = assetGrid ! 1 - assetGrid ! 0
        dT = timeGrid ! 1 - timeGrid ! 0
        
        -- Initial condition
        initialValues = payoff optType k assetGrid
        
        -- Coefficients for explicit part
        alpha = 0.5 * dT * r
        beta = 0.5 * dT * v * v
        
        -- Single time step evolution
        evolveStep values = V.generate (V.length values) $ \i ->
            if i == 0 || i == V.length values - 1
            then values ! i  -- Boundary conditions
            else
                let si = assetGrid ! i
                    delta = beta * (si * si) / (dS * dS)
                    mu = alpha * si / dS
                    prev = values ! (i-1)
                    curr = values ! i
                    next = values ! (i+1)
                in curr + delta * (next - 2 * curr + prev) + mu * (next - prev)
        
        -- Evolve through time with maximum iteration check
        maxIter = 100  -- Limit number of iterations
        timePoints = V.take maxIter timeGrid
        
        finalValues = V.foldl' (\vals _ -> evolveStep vals) initialValues timePoints
        
        -- Interpolate to get price at initial stock price
        idx = floor (s / dS)
        idx' = min (V.length finalValues - 2) idx  -- Ensure we don't go out of bounds
        x0 = assetGrid ! idx'
        x1 = assetGrid ! (idx' + 1)
        y0 = finalValues ! idx'
        y1 = finalValues ! (idx' + 1)
        
        -- Linear interpolation
        price = y0 + (s - x0) * (y1 - y0) / (x1 - x0)
    in 
        -- Apply discount factor
        exp (-r * t) * price