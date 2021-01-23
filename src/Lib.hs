module Lib
  ( draw,
  )
where

import Graphics.Image as I
import Hit
import Math.Vector
import Object
import Ray
import Prelude as P

data TempCamera = Camera (Vec3 Double) (Vec3 Double) (Vec3 Double) (Vec3 Double)

draw :: IO ()
draw = do
  let width = 1280
  let height = 720
  -- camera
  let vh = 2.0
  let vw = (fromIntegral width / fromIntegral height) * vh
  let focalLength = 1.0
  let origin = Vec3 0.0 0.0 0.0
  let horz = Vec3 vw 0.0 0.0
  let vert = Vec3 0.0 vh 0.0
  let corner = origin `subtr` (horz `divScalar` 2) `subtr` (vert `divScalar` 2) `subtr` Vec3 0 0 focalLength
  let camera = Camera origin horz vert corner
  let img = makeImageR VS (height, width) (getPixel camera)
  writeImage "test.png" img

getPixel :: TempCamera -> (Int, Int) -> Pixel RGB Double
getPixel (Camera origin w h corner) (row, col) =
  do
    let u = fromIntegral col / 1279
    let v = fromIntegral row / 719
    let ray = Ray origin (corner `add` scalarMul u w `add` scalarMul v h `subtr` origin)
    let (Vec3 r g b) = raytrace (Sphere (Vec3 0 0 (-1)) 0.5) ray
    PixelRGB r g b

raytrace :: Object -> Ray -> Color
raytrace s r = do
  let sphereHit = hit s r (-10) 999
  case sphereHit of
    Just _ -> Vec3 1.0 0.0 0.0
    Nothing -> do
      let (Vec3 _ y _) = unit $ direction r
      let t = 0.5 * (y + 1)
      (1 - t) `scalarMul` Vec3 1 1 1 `add` (t `scalarMul` Vec3 0.5 0.7 1.0)
