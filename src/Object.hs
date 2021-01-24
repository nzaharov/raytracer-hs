module Object where

import Material
import Math.Quadratic
import Math.Vector
import Ray

data Object = Object Geometry Material deriving (Show)

data Geometry = Sphere (Vec3 Double) Double deriving (Show)

instance Intersectable Object where
  intersect (Object (Sphere center r) mat) ray min max = do
    let oc = origin ray `subtr` center
    let a = normSqr $ direction ray
    let b = 2 `scalarMul` oc `dot` direction ray
    let c = normSqr oc - r * r
    roots <- solveQuardatic a b c
    rootMin <- smallerRootInInterval (min, max) roots
    let hitPoint = ray `at` rootMin
    let normal = (hitPoint `subtr` center) `divScalar` r
    Just $ Hit hitPoint normal rootMin mat
  intersect _ _ _ _ = undefined
