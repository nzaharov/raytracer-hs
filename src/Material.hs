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
  scatter :: a -> Ray -> Hit -> IO (Ray, Color)

newtype Material = Diffuse Color deriving (Show, Eq)

instance Material_ Material where
  scatter (Diffuse color) _ray hit = do
    randVector <- randInUnitSphere
    let scatterDirection = normal hit `add` unit randVector
    let scattered
          | isNearZero scatterDirection = Ray (point hit) scatterDirection
          | otherwise = Ray (point hit) $ normal hit
    return (scattered, color)

isNearZero :: Vec3 Double -> Bool
isNearZero (Vec3 x y z) =
  abs x < eps && abs y < eps && abs z < eps
  where
    eps = 1e-8