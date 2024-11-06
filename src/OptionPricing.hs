module OptionPricing (optionPayoff) where

import Types

optionPayoff :: OptionType -> StrikePrice -> Double -> Double
optionPayoff Call strike finalPrice = max (finalPrice - strike) 0
optionPayoff Put strike finalPrice  = max (strike - finalPrice) 0

