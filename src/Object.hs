{-# LANGUAGE NamedFieldPuns #-}

module Object where

import Control.Applicative
import Control.Monad
import Material
import Math.Quadratic
import Math.Vector
import Ray
import Utils

data Object = Object Geometry Material deriving (Show)

data Geometry
  = Sphere (Vec3 Double) Double
  | Plane (Vec3 Double) (Vec3 Double) (Vec3 Double)
  | Rect (Vec3 Double) (Vec3 Double) (Vec3 Double) (Vec3 Double)
  | Triangle (Vec3 Double) (Vec3 Double) (Vec3 Double)
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
  intersect (Rect a b c d) ray min max =
    intersect (Triangle a b c) ray min max
      <|> intersect (Triangle c d a) ray min max
  intersect (Triangle v1 v2 v3) ray min max = do
    let edge1 = v2 `subtr` v1
    let edge2 = v3 `subtr` v1
    let h = direction ray `cross` edge2
    let det = edge1 `dot` h
    maybeUnless (abs det < 1e-6) $ do
      -- check if parallel to plane
      let invDet = 1.0 / det
      let s = origin ray `subtr` v1
      let u = invDet * (s `dot` h)
      maybeUnless (u < 0 || u > 1) $ do
        let q = s `cross` edge1
        let v = invDet * direction ray `dot` q
        maybeUnless (v < 0 || u + v > 1) $ do
          let t = invDet * edge2 `dot` q
          maybeUnless (t < min || t > max) $
            Just $ Hit (ray `at` t) (edge1 `cross` edge2) t None
