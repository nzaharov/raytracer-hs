module Math.Utils where

import Control.Applicative (Alternative ((<|>)))

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

inInterval :: (Double, Double) -> Double -> Maybe Double
inInterval (a, b) x
  | x < a || x > b = Nothing
  | otherwise = Just x

smaller :: Double -> Double -> Double
smaller a b
  | a <= b = a
  | otherwise = b