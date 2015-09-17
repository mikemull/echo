module Echo.Types where

import System.Random


data Resource = A | B | C | D deriving (Enum, Show, Bounded, Eq, Ord)

instance Random Resource where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g


selfRepThreshold = 2 :: Int

selfRepFraction = 0.5

mutationProb = 0.001 :: Float
