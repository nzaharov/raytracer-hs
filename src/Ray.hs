module Ray where

import Math.Vector

type Color = Vec3 Double

data Ray = Ray (Vec3 Double) (Vec3 Double)

direction :: Ray -> Vec3 Double
direction (Ray _ direction) = direction

origin :: Ray -> Vec3 Double
origin (Ray origin _) = origin

at :: Ray -> Double -> Vec3 Double
at ray t = origin ray `add` (t `scalarMul` direction ray)