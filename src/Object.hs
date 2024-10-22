{-# LANGUAGE NamedFieldPuns #-}

module Object where

import Control.Applicative
import Data.List (minimumBy)
import Data.Maybe
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
  | Tetrahedron (Vec3 Double) (Vec3 Double) (Vec3 Double) (Vec3 Double)
  | Cuboid
      (Vec3 Double)
      (Vec3 Double)
      (Vec3 Double)
      (Vec3 Double)
      (Vec3 Double)
      (Vec3 Double)
      (Vec3 Double)
      (Vec3 Double)
  deriving (Show)

instance Intersectable Object where
  intersect (Object geometry mat) ray min max = do
    Hit {point, normal, t, mat = _} <- intersect geometry ray min max
    Just $ Hit point normal t mat

instance Intersectable Geometry where
  -- Sphere
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

  -- Plane
  intersect (Plane _p1 p2 normal) ray min max = do
    let denominator = direction ray `dot` normal
    let t = ((p2 `subtr` origin ray) `dot` normal) / denominator
    if t < min || t > max
      then Nothing
      else Just $ Hit (ray `at` t) normal t None

  -- Rectangle
  intersect (Rect a b c d) ray min max =
    intersect (Triangle a b c) ray min max
      <|> intersect (Triangle c d a) ray min max
  -- Triangle
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

  -- Tetrahedron
  intersect (Tetrahedron a b c d) ray min max =
    compositeIntersect
      [ Triangle a b c,
        Triangle c d a,
        Triangle a b d,
        Triangle d c b
      ]
      ray
      min
      max
  -- Cuboid
  intersect (Cuboid a1 a2 a3 a4 b1 b2 b3 b4) ray min max =
    compositeIntersect -- this can probably be generalized more
      [ Rect a1 a2 a3 a4,
        Rect b1 b2 b3 b4,
        Rect a1 a2 b2 b1,
        Rect a3 a4 b4 b3,
        Rect a2 a3 b3 b2,
        Rect a1 a4 b4 b1
      ]
      ray
      min
      max

compositeIntersect :: Intersectable a => [a] -> Ray -> Double -> Double -> Maybe Hit
compositeIntersect figures ray min max
  | null hits = Nothing
  | otherwise = Just $ minimumBy (\a b -> compare (t a) (t b)) hits
  where
    hits = mapMaybe (\figure -> intersect figure ray min max) figures