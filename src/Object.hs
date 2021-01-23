module Object where

import Hit
import Material
import Math.Quadratic
import Math.Vector
import Ray

data Object = Sphere (Vec3 Double) Double Diffuse deriving (Show)

instance Intersectable Object where
  intersect rng (Sphere center r mat) ray min max = do
    let oc = origin ray `subtr` center
    let a = normSqr $ direction ray
    let b = 2 `scalarMul` oc `dot` direction ray
    let c = normSqr oc - r * r
    roots <- solveQuardatic a b c
    rootMin <- smallerRootInInterval (min, max) roots
    let hitPoint = ray `at` rootMin
    let normal = (hitPoint `subtr` center) `divScalar` r
    scatter mat rng ray (Hit hitPoint normal rootMin)
  intersect _ _ _ _ _ = undefined
