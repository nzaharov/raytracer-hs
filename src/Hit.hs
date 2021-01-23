module Hit where

import Math.Vector
import Ray
import System.Random

data Hit = Hit
  { point :: Vec3 Double,
    normal :: Vec3 Double,
    t :: Double
  }
  deriving (Eq)

class Intersectable a where
  intersect :: StdGen -> a -> Ray -> Double -> Double -> Maybe (Ray, Color)