module Lib
  ( draw,
  )
where

import Data.Foldable
import GHC.IO
import Graphics.Image as I
import Material
import Math.Utils
import Math.Vector
import Object
import Ray
import Scene
import System.Random
import Prelude as P

data TempCamera = Camera (Vec3 Double) (Vec3 Double) (Vec3 Double) (Vec3 Double)

width :: Int
width = 320

height :: Int
height = 180

sampleSize :: Int
sampleSize = 50

draw :: IO ()
draw = do
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
  putStrLn "Rendering..."
  pixels <- mapM (getPixel camera scene) [(j, i) | j <- [0 .. height - 1], i <- [0 .. width - 1]]
  putStrLn "Saving image..."
  writeImage "test.png" $ flipV ((fromLists $ chunks width pixels) :: Image RSU RGB Double)
  putStrLn "Done!"

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n l
  | n > 0 = take n l : chunks n (drop n l)
  | otherwise = error "Negative or zero n"

getPixel :: TempCamera -> Scene Object -> (Int, Int) -> IO (Pixel RGB Double)
getPixel (Camera origin w h corner) scene (row, col) =
  do
    let f = \acc _ ->
          do
            r1 <- randomIO
            r2 <- randomIO
            let u = (fromIntegral col + r1) / (fromIntegral width - 1)
            let v = (fromIntegral row + r2) / (fromIntegral height - 1)
            let ray =
                  Ray
                    origin
                    (corner `add` scalarMul u w `add` scalarMul v h `subtr` origin)
            color <- raytrace scene ray 50
            return $ acc `add` color

    (Vec3 r g b) <- processPixelData sampleSize <$> foldlM f (Vec3 0 0 0) [1 .. sampleSize]
    return $
      PixelRGB r g b

processPixelData :: Int -> Color -> Color
processPixelData samples (Vec3 r g b) = Vec3 (correct r) (correct g) (correct b)
  where
    scale = 1.0 / fromIntegral samples
    correct = \x -> clamp (sqrt $ scale * x) (0, 0.999)

raytrace :: Scene Object -> Ray -> Int -> IO Color
raytrace scene ray depth = do
  if depth <= 0
    then return black
    else do
      let sphereHit = intersect scene ray 0.001 100000000000
      case sphereHit of
        Just hit -> do
          (scattered, color) <- scatter (mat hit) ray hit
          childRayColor <- raytrace scene scattered (depth - 1)
          return $ color `mul` childRayColor
        Nothing -> return $ sky ray

sky :: Ray -> Color
sky r = do
  let (Vec3 _ y _) = unit $ direction r
  let t = 0.5 * (y + 1)
  (1 - t) `scalarMul` Vec3 1 1 1 `add` (t `scalarMul` Vec3 0.5 0.7 1.0)

genScene :: Scene Object
genScene =
  Scene
    [ Object (Sphere (Vec3 0 0 (-1)) 0.5) (Diffuse $ Vec3 0.8 0.8 0),
      Object (Sphere (Vec3 0 (-100.5) (-1)) 100) (Diffuse $ Vec3 0.1 0.1 0.7),
      Object (Sphere (Vec3 1.0 0 (-2)) 0.5) (Diffuse $ Vec3 1 0 0),
      Object (Sphere (Vec3 (-2) 0 (-1.5)) 0.5) (Diffuse $ Vec3 0.8 0.8 0),
      Object (Sphere (Vec3 0.27 0.1 (-0.5)) 0.05) (Diffuse $ Vec3 1 1 1)
    ]