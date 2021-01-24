module ImageSettings where

data ImageSettings = ImageSettings
  { width :: Int,
    height :: Int,
    filename :: String,
    samples :: Int
  }
  deriving (Show)

aspectRatio :: ImageSettings -> Double
aspectRatio (ImageSettings width height _ _) =
  fromIntegral width / fromIntegral height