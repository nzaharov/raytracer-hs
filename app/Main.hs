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
setImage = ImageSettings 640 360 "output/scene.png" 20

setCamera :: Camera
setCamera =
  Camera
    -- rotation
    -- (Vec3 0.5 0.5 0)
    -- (Vec3 0.0 0.0 (-1.0))
    -- (Vec3 0.0 1.0 0.0)
    -- front
    (Vec3 0 0 0)
    (Vec3 0.0 0.0 (-1.0))
    (Vec3 0.0 1.0 0.0)
    -- top view
    -- (Vec3 0 5 (-1.5))
    -- (Vec3 0 0 (-1.5))
    -- (Vec3 0 0 (-1.5))
    75.0
    (16.0 / 9.0)
    0.01
    0.5

genScene :: Scene Object
genScene =
  Scene
    [ Object (Sphere (Vec3 0 0 (-1)) 0.5) (Diffuse $ Vec3 0.8 0.8 0),
      Object
        (Rect (Vec3 (-1) 0.7 (-1)) (Vec3 (-0.5) 0.7 (-1.3)) (Vec3 (-0.5) 0.2 (-1.3)) (Vec3 (-1) 0.2 (-1)))
        (Diffuse $ Vec3 0.8 0 0),
      Object
        (Plane (Vec3 1 1 1) (Vec3 0 (-0.5) 0) (Vec3 0 1 0))
        (Metal (Vec3 0.1 0.1 0.7) 0.5),
      Object (Sphere (Vec3 1.0 0 (-2)) 0.5) (Diffuse black),
      Object (Sphere (Vec3 (-2) 0 (-1.5)) 0.5) (Metal (Vec3 0.8 0.8 0.8) 0),
      Object (Sphere (Vec3 0.27 0.1 (-0.5)) 0.05) (Glass 0.5),
      Object (Triangle (Vec3 0 2.5 0) (Vec3 0 2 (-1)) (Vec3 1 2.5 0)) $ Diffuse black,
      Object
        (Tetrahedron (Vec3 (-1) (-0.4) (-1)) (Vec3 (-0.7) (-0.4) (-1)) (Vec3 (-0.85) (-0.4) (-1.3)) (Vec3 (-0.85) 0.0 (-1.1)))
        (Diffuse $ Vec3 0.2 0.6 0)
    ]