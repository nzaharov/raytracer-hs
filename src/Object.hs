module Object where

import Hit
import Math.Utils
import Math.Vector
import Ray

data Object = Sphere (Vec3 Double) Double deriving (Show)

instance Hittable Object where
  hit (Sphere center r) ray min max = do
    let oc = origin ray `subtr` center
    let a = normSqr $ direction ray
    let b = 2 `scalarMul` oc `dot` direction ray
    let c = normSqr oc - r * r
    roots <- solveQuardatic a b c
    rootMin <- smallerRootInInterval (min, max) roots
    let hitPoint = ray `at` rootMin
    let normal = (hitPoint `subtr` center) `divScalar` r
    Just $ Hit hitPoint normal rootMin
  hit _ _ _ _ = undefined
