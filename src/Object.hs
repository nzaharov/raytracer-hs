module Object where

import Material
import Math.Quadratic
import Math.Vector
import Ray

data Object = Object Geometry Material deriving (Show)

data Geometry
  = Sphere (Vec3 Double) Double
  | Plane (Vec3 Double) (Vec3 Double) (Vec3 Double)
  deriving (Show)

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
  intersect (Object (Plane _p1 p2 normal) mat) ray min max = do
    let denominator = direction ray `dot` normal
    let t = ((p2 `subtr` origin ray) `dot` normal) / denominator
    if t < min || t > max
      then Nothing
      else Just $ Hit (ray `at` t) normal t mat