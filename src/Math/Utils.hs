module Math.Utils where

inInterval :: (Double, Double) -> Double -> Maybe Double
inInterval (a, b) x
  | x < a || x > b = Nothing
  | otherwise = Just x

smaller :: Double -> Double -> Double
smaller a b
  | a <= b = a
  | otherwise = b

clamp :: Ord a => a -> (a, a) -> a
clamp x (a, b)
  | x > b = b
  | x < a = a
  | otherwise = x

toRadians :: Double -> Double
toRadians deg = pi * deg / 180