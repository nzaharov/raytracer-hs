module Main where

import Camera
import ImageSettings
import Lib
import Material
import Math.Vector
import Object
import Ray
import Scene

main :: IO ()
main = render setImage setCamera genScene

setImage :: ImageSettings
setImage = ImageSettings 640 360 "balls.png" 50

setCamera :: Camera
setCamera =
  Camera
    (Vec3 0.0 0.0 0.0)
    (Vec3 0.0 0.0 (-1.0))
    (Vec3 0.0 1.0 0.0)
    75.0
    (16.0 / 9.0)
    0.01
    0.5

genScene :: Scene Object
genScene =
  Scene
    [ Object (Sphere (Vec3 0 0 (-1)) 0.5) (Diffuse $ Vec3 0.8 0.8 0),
      Object (Sphere (Vec3 0 (-100.5) (-1)) 100) (Metal (Vec3 0.1 0.1 0.7) 0.5),
      Object (Sphere (Vec3 1.0 0 (-2)) 0.5) (Diffuse black),
      Object (Sphere (Vec3 (-2) 0 (-1.5)) 0.5) (Metal (Vec3 0.8 0.8 0.8) 0),
      Object (Sphere (Vec3 0.27 0.1 (-0.5)) 0.05) (Glass 0.5)
    ]