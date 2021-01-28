module Math.Vector where

import System.Random

data Vec3 a = Vec3 a a a deriving (Show, Eq)

dimX :: Vec3 a -> a
dimX (Vec3 x _ _) = x

dimY :: Vec3 a -> a
dimY (Vec3 _ y _) = y

dimZ :: Vec3 a -> a
dimZ (Vec3 _ _ z) = z

add :: Num a => Vec3 a -> Vec3 a -> Vec3 a
(Vec3 x y z) `add` (Vec3 x' y' z') = Vec3 (x + x') (y + y') (z + z')

subtr :: Num a => Vec3 a -> Vec3 a -> Vec3 a
(Vec3 x y z) `subtr` (Vec3 x' y' z') = Vec3 (x - x') (y - y') (z - z')

mul :: Num a => Vec3 a -> Vec3 a -> Vec3 a
(Vec3 x y z) `mul` (Vec3 x' y' z') = Vec3 (x * x') (y * y') (z * z')

neg :: Num a => Vec3 a -> Vec3 a
neg (Vec3 x y z) = Vec3 (- x) (- y) (- z)

scalarMul :: Num a => a -> Vec3 a -> Vec3 a
c `scalarMul` Vec3 x y z = Vec3 (c * x) (c * y) (c * z)

divScalar :: Fractional a => Vec3 a -> a -> Vec3 a
Vec3 x y z `divScalar` c = Vec3 (x / c) (y / c) (z / c)

normSqr :: Num a => Vec3 a -> a
normSqr (Vec3 x y z) = x * x + y * y + z * z

norm :: Floating a => Vec3 a -> a
norm vec = sqrt $ normSqr vec

unit :: Floating a => Vec3 a -> Vec3 a
unit vec = vec `divScalar` norm vec

dot :: Num a => Vec3 a -> Vec3 a -> a
dot (Vec3 x y z) (Vec3 x' y' z') = x * x' + y * y' + z * z'

cross :: Num a => Vec3 a -> Vec3 a -> Vec3 a
cross (Vec3 x y z) (Vec3 x' y' z') =
  Vec3
    (y * z' - z * y')
    (z * x' - x * z')
    (x * y' - y * x')

randInSphere :: (Double, Double) -> IO (Vec3 Double)
randInSphere range = do
  x <- randomRIO range
  y <- randomRIO range
  z <- randomRIO range
  return $ Vec3 x y z

randInUnitSphere :: IO (Vec3 Double)
randInUnitSphere = untilUnit (-1, 1) randInSphere

randInUnitDisc :: IO (Vec3 Double)
randInUnitDisc = untilUnit (-1, 1) randInDisc

randInDisc :: (Double, Double) -> IO (Vec3 Double)
randInDisc range = do
  x <- randomRIO range
  y <- randomRIO range
  return $ Vec3 x y 0

untilUnit :: a -> (a -> IO (Vec3 Double)) -> IO (Vec3 Double)
untilUnit a g = do
  initialRand <- g a
  loop initialRand
  where
    loop vec = do
      if normSqr vec < 1
        then return vec
        else do
          rand <- g a
          loop rand

randInUnitHemi :: Vec3 Double -> IO (Vec3 Double) -- can be used as an alternative for Lambertian diffuse
randInUnitHemi normal = do
  inSphere <- randInUnitSphere
  if normal `dot` inSphere > 0
    then return inSphere
    else return $ neg inSphere