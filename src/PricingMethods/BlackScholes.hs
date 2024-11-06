module PricingMethods.BlackScholes (blackScholesOption) where

import Types

-- Black-Scholes formula for European Call and Put options
blackScholesOption :: InitialStockPrice -> StrikePrice -> InterestRate -> Volatility -> TimeToMaturity -> OptionType -> Double
blackScholesOption s k r v t Call = s * normCDF d1 - k * exp (-r * t) * normCDF d2
  where
    d1 = (log (s / k) + (r + 0.5 * v^2) * t) / (v * sqrt t)
    d2 = d1 - v * sqrt t

blackScholesOption s k r v t Put = k * exp (-r * t) * normCDF (-d2) - s * normCDF (-d1)
  where
    d1 = (log (s / k) + (r + 0.5 * v^2) * t) / (v * sqrt t)
    d2 = d1 - v * sqrt t

-- Approximation of the cumulative distribution function for a standard normal distribution
normCDF :: Double -> Double
normCDF x = 0.5 * (1 + erf (x / sqrt 2))

-- Approximation of the error function (erf)
erf :: Double -> Double
erf z = signum z * sqrt (1 - exp (-z * z * (4 / pi + a * z * z) / (1 + a * z * z)))
  where
    a = 0.147  -- Approximation constant

