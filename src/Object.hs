{-# LANGUAGE NamedFieldPuns #-}

module Object where

import Material
import Math.Quadratic
import Math.Vector
import Ray

data Object = Object Geometry Material deriving (Show)

data Geometry
  = Sphere (Vec3 Double) Double
  | Plane (Vec3 Double) (Vec3 Double) (Vec3 Double)
  | Wall (Vec3 Double) (Vec3 Double) (Vec3 Double) (Vec3 Double)
  deriving (Show)

instance Intersectable Object where
  intersect (Object geometry mat) ray min max = do
    Hit {point, normal, t, mat = _} <- intersect geometry ray min max
    Just $ Hit point normal t mat

instance Intersectable Geometry where
  intersect (Sphere center r) ray min max = do
    let oc = origin ray `subtr` center
    let a = normSqr $ direction ray
    let b = 2 `scalarMul` oc `dot` direction ray
    let c = normSqr oc - r * r
    roots <- solveQuardatic a b c
    rootMin <- smallerRootInInterval (min, max) roots
    let hitPoint = ray `at` rootMin
    let normal = (hitPoint `subtr` center) `divScalar` r
    Just $ Hit hitPoint normal rootMin None
  intersect (Plane _p1 p2 normal) ray min max = do
    let denominator = direction ray `dot` normal
    let t = ((p2 `subtr` origin ray) `dot` normal) / denominator
    if t < min || t > max
      then Nothing
      else Just $ Hit (ray `at` t) normal t None
  intersect (Wall a b c d) ray min max = do
    -- a - b
    -- d - c
    let normal = (a `subtr` c) `cross` (b `subtr` d)
    hit <- intersect (Plane a b normal) ray min max
    let (Vec3 x y _) = point hit
    -- TODO account for axis orientation
    if x >= dimX a && x <= dimX b && y <= dimY b && y >= dimY c
      then Just hit
      else Nothing
