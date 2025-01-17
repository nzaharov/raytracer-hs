module Math.Quadratic where

import Control.Applicative (Alternative ((<|>)))
import Math.Utils ( inInterval, smaller )

data Roots = One Double | Two Double Double deriving (Show)

solveQuardatic :: Double -> Double -> Double -> Maybe Roots
solveQuardatic a b c
  | discriminant < 0 = Nothing
  | discriminant == 0 = Just $ One (- b / (2 * a))
  | discriminant > 0 =
    Just $
      Two
        ((- b + sqrt discriminant) / (2 * a))
        ((- b - sqrt discriminant) / (2 * a))
  where
    discriminant = b * b - 4 * a * c

smallerRootInInterval :: (Double, Double) -> Roots -> Maybe Double
smallerRootInInterval ab (One x) = inInterval ab x
smallerRootInInterval ab (Two x y) =
  smaller <$> inAB x <*> inAB y
    <|> inAB x
    <|> inAB y
  where
    inAB = inInterval ab