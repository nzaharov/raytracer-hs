module Material where

import Hit
import Math.Vector
import Ray
import System.Random

class Material a where
  scatter :: a -> StdGen -> Ray -> Hit -> Maybe (Ray, Color)

newtype Diffuse = Diffuse Color deriving (Show)

instance Material Diffuse where
  scatter (Diffuse color) rng _ray hit = do
    let scatterDirection = normal hit `add` unit (randInUnitSphere rng)
    let scattered = Ray (point hit) scatterDirection
    Just (scattered, color)