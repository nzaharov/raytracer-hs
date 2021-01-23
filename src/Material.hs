module Material where

import Math.Vector
import Ray
import System.Random

data Hit = Hit
  { point :: Vec3 Double,
    normal :: Vec3 Double,
    t :: Double,
    mat :: Material
  }
  deriving (Eq)

class Intersectable a where
  intersect :: a -> Ray -> Double -> Double -> Maybe Hit

class Material_ a where
  scatter :: a -> StdGen -> Ray -> Hit -> (Ray, Color)

newtype Material = Diffuse Color deriving (Show, Eq)

instance Material_ Material where
  scatter (Diffuse color) rng _ray hit = do
    let scatterDirection = normal hit `add` unit (randInUnitSphere rng)
    let scattered = Ray (point hit) scatterDirection
    (scattered, color)