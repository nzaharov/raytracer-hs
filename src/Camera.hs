module Camera (Camera (Camera), getRay) where

import Math.Utils
import Math.Vector
import Ray

data Camera = Camera
  { lookfrom :: Vec3 Double,
    lookat :: Vec3 Double,
    vup :: Vec3 Double,
    verticalFoV :: Double,
    aspectRatio :: Double,
    aperature :: Double,
    focusDistance :: Double -- distance where focus is best
  }
  deriving (Show)

getRay :: Camera -> Double -> Double -> IO Ray
getRay cam dX dY = do
  (Vec3 x y _z) <- scalarMul (lensRadius cam) <$> randInUnitDisc
  let offset = (x `scalarMul` u cam) `add` (y `scalarMul` v cam)
  let orig = lookfrom cam `add` offset
  let dir =
        lowerLeftCorner cam
          `add` (dX `scalarMul` horizontal cam)
          `add` (dY `scalarMul` vertical cam)
          `subtr` orig
  return $ Ray orig dir

viewportHeight :: Camera -> Double
viewportHeight (Camera _ _ _ verticalFoV _ _ _) = 2 * h
  where
    h = tan (toRadians verticalFoV / 2)

viewportWidth :: Camera -> Double
viewportWidth cam = aspectRatio cam * viewportHeight cam

lensRadius :: Camera -> Double
lensRadius (Camera _ _ _ _ _ aperature _) = aperature / 2

lowerLeftCorner :: Camera -> Vec3 Double
lowerLeftCorner cam =
  lookfrom cam `subtr` (horizontal cam `divScalar` 2)
    `subtr` (vertical cam `divScalar` 2)
    `subtr` (focusDistance cam `scalarMul` w cam)

horizontal :: Camera -> Vec3 Double
horizontal cam = (focusDistance cam * viewportWidth cam) `scalarMul` u cam

vertical :: Camera -> Vec3 Double
vertical cam = (focusDistance cam * viewportHeight cam) `scalarMul` v cam

-- Base vectors
w :: Camera -> Vec3 Double
w (Camera lookfrom lookat _ _ _ _ _) = unit (lookfrom `subtr` lookat)

u :: Camera -> Vec3 Double
u cam = unit $ vup cam `cross` w cam

v :: Camera -> Vec3 Double
v cam = w cam `cross` u cam