module Lib
  ( draw,
  )
where

import Camera
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
import Utils
import Prelude as P

width :: Int
width = 320

height :: Int
height = 180

sampleSize :: Int
sampleSize = 50

draw :: IO ()
draw = do
  let aspectRatio = fromIntegral width / fromIntegral height
  let camera =
        Camera
          (Vec3 0.0 0.0 0.0)
          (Vec3 0.0 0.0 (-1.0))
          (Vec3 0.0 1.0 0.0)
          75.0
          aspectRatio
          0.01
          0.5
  let scene = genScene
  putStrLn "Rendering..."
  pixels <- mapM (getPixel camera scene) [(j, i) | j <- [0 .. height - 1], i <- [0 .. width - 1]]
  putStrLn "Saving image..."
  writeImage "test.png" $ flipV ((fromLists $ chunks width pixels) :: Image RSU RGB Double)
  putStrLn "Done!"

getPixel :: Camera -> Scene Object -> (Int, Int) -> IO (Pixel RGB Double)
getPixel cam scene (row, col) =
  do
    let f = \acc _ ->
          do
            r1 <- randomIO
            r2 <- randomIO
            let u = (fromIntegral col + r1) / (fromIntegral width - 1)
            let v = (fromIntegral row + r2) / (fromIntegral height - 1)
            ray <- getRay cam u v
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
      Object (Sphere (Vec3 0 (-100.5) (-1)) 100) (Metal (Vec3 0.1 0.1 0.7) 0.5),
      Object (Sphere (Vec3 1.0 0 (-2)) 0.5) (Diffuse black),
      Object (Sphere (Vec3 (-2) 0 (-1.5)) 0.5) (Metal (Vec3 0.8 0.8 0.8) 0),
      Object (Sphere (Vec3 0.27 0.1 (-0.5)) 0.05) (Glass 0.5)
    ]