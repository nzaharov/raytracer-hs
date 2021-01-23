module Lib
  ( draw,
  )
where

import Graphics.Image as I
import Material
import Math.Vector
import Object
import Ray
import Scene
import System.Random
import Prelude as P

data TempCamera = Camera (Vec3 Double) (Vec3 Double) (Vec3 Double) (Vec3 Double)

draw :: IO ()
draw = do
  rng <- getStdGen
  let width = 1280
  let height = 720
  -- camera
  let vh = 2.0
  let vw = (fromIntegral width / fromIntegral height) * vh
  let focalLength = 1.0
  let origin = Vec3 0.0 0.0 0.0
  let horz = Vec3 vw 0.0 0.0
  let vert = Vec3 0.0 vh 0.0
  let corner =
        origin
          `subtr` (horz `divScalar` 2)
          `subtr` (vert `divScalar` 2)
          `subtr` Vec3 0 0 focalLength
  let camera = Camera origin horz vert corner
  let scene = genScene
  let img = flipV $ makeImageR VS (height, width) (getPixel rng camera scene)
  writeImage "test.png" img

getPixel :: StdGen -> TempCamera -> Scene Object -> (Int, Int) -> Pixel RGB Double
getPixel rng (Camera origin w h corner) scene (row, col) =
  do
    let u = fromIntegral col / 1279
    let v = fromIntegral row / 719
    let ray = Ray origin (corner `add` scalarMul u w `add` scalarMul v h `subtr` origin)
    let (Vec3 r g b) = raytrace rng scene ray 50
    PixelRGB r g b

raytrace :: StdGen -> Scene Object -> Ray -> Int -> Color
raytrace rng scene ray depth = do
  if depth <= 0
    then black
    else do
      let sphereHit = intersect scene ray 0.001 100000000000
      case sphereHit of
        Just hit -> do
          let (scattered, color) = scatter (mat hit) rng ray hit
          color `mul` raytrace rng scene scattered (depth - 1)
        Nothing -> sky ray

sky :: Ray -> Color
sky r = do
  let (Vec3 _ y _) = unit $ direction r
  let t = 0.5 * (y + 1)
  (1 - t) `scalarMul` Vec3 1 1 1 `add` (t `scalarMul` Vec3 0.5 0.7 1.0)

genScene :: Scene Object
genScene =
  Scene
    [ Object (Sphere (Vec3 0 0 (-1)) 0.5) (Diffuse $ Vec3 0.8 0.8 0),
      Object (Sphere (Vec3 0 (-100.5) (-1)) 100) (Diffuse $ Vec3 0.1 0.6 0.1)
    ]