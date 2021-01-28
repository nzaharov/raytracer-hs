module Material where

import Math.Utils (smaller)
import Math.Vector
import Ray
import System.Random (Random (randomIO))

data Hit = Hit
  { point :: Vec3 Double,
    normal :: Vec3 Double,
    t :: Double,
    mat :: Material
  }
  deriving (Show, Eq)

isOnFront :: Hit -> Ray -> Bool
isOnFront hit ray = direction ray `dot` normal hit < 0

class Intersectable a where
  intersect :: a -> Ray -> Double -> Double -> Maybe Hit

class Material_ a where
  scatter :: a -> Ray -> Hit -> IO (Ray, Color)

data Material
  = None
  | Diffuse Color
  | Metal Color Double
  | Glass Double
  deriving (Show, Eq)

instance Material_ Material where
  scatter (Diffuse color) _ hit = do
    randVector <- randInUnitSphere
    let scatterDirection = normal hit `add` unit randVector
    let scattered
          | isNearZero scatterDirection = Ray (point hit) $ normal hit
          | otherwise = Ray (point hit) scatterDirection
    return (scattered, color)
  scatter (Metal color fuzz) ray hit = do
    let reflected = reflect (normal hit) $ unit $ direction ray
    fuzzed <- scalarMul fuzz <$> randInUnitSphere
    let scattered = Ray (point hit) (reflected `add` fuzzed)
    return (scattered, color)
  scatter (Glass refrIndex) ray hit = do
    let refrRatio -- eta / eta'
          | isOnFront hit ray = 1 / refrIndex
          | otherwise = refrIndex
    let rayDirection = unit $ direction ray
    let cosAngle = neg rayDirection `dot` normal hit `smaller` 1
    let sinAngle = sqrt (1 - cosAngle * cosAngle)
    let cannotRefract = refrRatio * sinAngle > 1
    chance <- randomIO :: IO Double
    let shouldReflect = reflectance cosAngle refrRatio > chance
    let scatterDirection
          | cannotRefract || shouldReflect = reflect (normal hit) rayDirection
          | otherwise = refract refrRatio (normal hit) rayDirection
    return (Ray (point hit) scatterDirection, Vec3 1 1 1)

isNearZero :: Vec3 Double -> Bool
isNearZero (Vec3 x y z) =
  abs x < eps && abs y < eps && abs z < eps
  where
    eps = 1e-10

reflect :: Vec3 Double -> Vec3 Double -> Vec3 Double
reflect axis vec = vec `subtr` ((2 * (vec `dot` axis)) `scalarMul` axis)

refract :: Double -> Vec3 Double -> Vec3 Double -> Vec3 Double
refract ratio axis vec = do
  let cosAngle = vec `dot` axis `smaller` 1
  let rPerp = ratio `scalarMul` (vec `add` (cosAngle `scalarMul` axis))
  let rPar = (- (sqrt $ abs (1 - normSqr rPerp))) `scalarMul` axis
  rPerp `add` rPar

reflectance :: Double -> Double -> Double
reflectance cosAngle refrRatio = do
  let r0 = (1 - refrRatio) / (1 + refrRatio)
  let r0Sqr = r0 * r0
  r0Sqr + (1 - r0Sqr) * ((1 - cosAngle) ** 5)