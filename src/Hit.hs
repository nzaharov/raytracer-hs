module Hit where

import Ray
import Math.Vector

data Hit = Hit
  { point :: Vec3 Double,
    normal :: Vec3 Double,
    t :: Double
  }
  deriving (Eq)

class Hittable a where
  hit :: a -> Ray -> Double -> Double -> Maybe Hit