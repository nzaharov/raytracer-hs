module Lib
  ( draw,
  )
where

import Graphics.Image as I
import Ray
import Vector
import Prelude as P

data TempCamera = Camera (Vec3 Double) (Vec3 Double) (Vec3 Double) (Vec3 Double)

-- ray r(origin, lower_left_corner + u*horizontal + v*vertical - origin);

draw :: IO ()
draw = do
  let width = 1280
  let height = 720
  -- camera
  let vh = 2.0
  let vw = ((fromIntegral width) / fromIntegral height) * vh
  let focalLength = 1.0
  let origin = Vec3 0.0 0.0 0.0
  let horz = Vec3 vw 0.0 0.0
  let vert = Vec3 0.0 vh 0.0
  let corner = origin `add` (horz `divScalar` 2) `subtr` (vert `divScalar` 2) `subtr` Vec3 0 0 focalLength
  let camera = Camera origin horz vert corner
  let img = makeImageR VS (height, width) (getPixel camera)
  writeImage "test.png" img

getPixel :: TempCamera -> (Int, Int) -> Pixel RGB Double
getPixel (Camera origin w h corner) (row, col) =
  do
    let v = (fromIntegral row) / 720
    let u = (fromIntegral col) / 1280
    let ray = Ray origin (corner `add` (scalarMul u w) `add` (scalarMul v h) `subtr` origin)
    let (Vec3 r g b) = raytrace ray
    PixelRGB r g b

raytrace :: Ray -> Color
raytrace r = do
  let (Vec3 _ y _) = unit $ direction r
  let t = 0.5 * (y + 1)
  (1 - t) `scalarMul` (Vec3 1 1 1) `add` (t `scalarMul` (Vec3 0.5 0.7 1.0))
